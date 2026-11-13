# Module: Session Management
# Connect, disconnect, and query status of OMOP CDM DataSHIELD sessions.

# --- JSON transport for Opal compatibility ---

#' Recursively coerce named atomic vectors to lists
#'
#' \code{jsonlite::toJSON} drops the names of a named atomic vector (it emits a
#' bare array), but keeps the names of a list (as object keys). Walking the
#' structure and converting every named atomic vector to a list makes names
#' survive the JSON round-trip at any nesting depth. Unnamed atomic vectors are
#' left untouched so they stay JSON arrays, and data frames are left to
#' \code{toJSON}'s native row-wise encoding.
#'
#' @param x An R object (list, vector, or scalar).
#' @return \code{x} with every named atomic vector turned into a named list.
#' @keywords internal
.ds_coerce_names <- function(x) {
  if (is.data.frame(x)) return(x)
  if (is.list(x)) return(lapply(x, .ds_coerce_names))
  if (is.atomic(x) && !is.null(names(x))) return(as.list(x))
  x
}

#' Encode a complex R object as JSON for DataSHIELD transport
#'
#' When passing complex R objects (lists, named vectors) through
#' datashield.assign.expr() or datashield.aggregate(), Opal serializes them
#' via deparse(), which generates structure()/c() calls not in the
#' DataSHIELD whitelist. This helper wraps them as JSON strings.
#' The server-side .ds_arg() transparently deserializes them.
#' In DSLite, call() handles native R objects directly, so this
#' function only encodes lists/complex objects — scalars pass through.
#'
#' @param x An R object to encode.
#' @return A JSON string if x is a list, or x unchanged if scalar.
#' @keywords internal
.ds_encode <- function(x) {
  if (is.list(x) || (is.vector(x) && length(x) > 1)) {
    # jsonlite::toJSON serializes a named ATOMIC vector as a bare JSON array,
    # silently dropping its names (a named LIST keeps them as object keys).
    # This bites the execute out-mapping c(demo = "D", drugs = "X") and any
    # named vector a caller nests inside a plan/spec (e.g. a tar or
    # time_window written as c(start = 0, end = 365)). Recursively coerce
    # every named atomic vector to a list so names survive transport; the
    # server-side .ds_arg() reads them back as object keys.
    x <- .ds_coerce_names(x)
    json <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
    # URL-safe base64: no +/= that could confuse Opal's R expression parser
    b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
    b64 <- gsub("\\+", "-", b64)
    b64 <- gsub("/", "_", b64)
    b64 <- gsub("=+$", "", b64)
    paste0("B64:", b64)
  } else {
    x
  }
}

#' Resilient datashield.aggregate that tolerates per-server failures
#'
#' Calls each server individually and returns partial results when some
#' servers fail (e.g., table not present on one server). Failed servers
#' — including a missing or NULL DSI response — are omitted from the result and
#' their errors are attached as an attribute. A server reporting that the
#' OHDSI community tools are disabled stops the operation with that reason.
#'
#' @param conns DSI connections object.
#' @param expr The call expression to evaluate.
#' @return Named list of results (only successful servers).
#' @keywords internal
.ds_safe_aggregate <- function(conns, expr) {
  server_names <- names(conns)
  results <- list()
  errors <- list()
  for (srv in server_names) {
    tryCatch({
      res <- DSI::datashield.aggregate(
        conns[srv], expr = expr,
        error = function(server, message) {
          errors[[server]] <<- message
        }
      )
      if (!is.list(res) || !srv %in% names(res) || is.null(res[[srv]])) {
        stop("server returned no verifiable aggregate result", call. = FALSE)
      }
      results[[srv]] <- res[[srv]]
    }, error = function(e) {
      errors[[srv]] <<- errors[[srv]] %||% conditionMessage(e)
    })
  }
  if (length(errors) > 0) {
    disabled <- names(errors)[vapply(errors, function(error) {
      grepl("community tools", error, fixed = TRUE) &&
        grepl("are disabled on this server", error, fixed = TRUE)
    }, logical(1L))]
    if (length(disabled) > 0L) {
      stop("The OHDSI community tools (Achilles, OHDSI results, the analysis ",
           "catalog and the query library) are disabled on: ",
           paste(disabled, collapse = ", "), ". The data custodian can enable ",
           "them again with the dsomop.tools.enabled option.", call. = FALSE)
    }
    attr(results, "ds_errors") <- errors
  }
  results
}

#' Verify that generic raw-container AggregateMethods are unavailable
#'
#' A DataSHIELD aggregate method mapped directly to base \code{c} or
#' \code{list} can wrap a protected server object and return it without a
#' reviewed disclosure gate. Connection therefore fails before resource
#' assignment unless every server provides a verifiable aggregate-method
#' inventory free of both direct names and aliases to those functions.
#'
#' @param conns Named DataSHIELD connections.
#' @return The verified method inventory, invisibly.
#' @keywords internal
.assert_safe_aggregate_methods <- function(conns) {
  expected <- names(conns)
  required <- c("server", "name", "type", "class", "value", "package",
                "version")
  methods <- tryCatch(
    DSI::datashield.methods(conns, type = "aggregate"),
    error = identity
  )
  if (inherits(methods, "error")) {
    stop("Could not verify DataSHIELD AggregateMethods before OMOP resource ",
         "assignment: ", conditionMessage(methods), ".", call. = FALSE)
  }
  complete_column <- function(x) {
    !anyNA(x) && all(nzchar(trimws(as.character(x))))
  }
  if (!is.data.frame(methods) || !all(required %in% names(methods)) ||
      !all(vapply(methods[intersect(required, names(methods))],
                  complete_column, logical(1L)))) {
    stop("Could not verify DataSHIELD AggregateMethods: inventory must follow ",
         "the complete DSI contract (server, name, type, class, value, ",
         "package, version).",
         call. = FALSE)
  }
  method_types <- tolower(trimws(as.character(methods$type)))
  if (any(method_types != "aggregate")) {
    stop("Could not verify DataSHIELD AggregateMethods: inventory contains ",
         "non-aggregate method rows.", call. = FALSE)
  }

  inventory_servers <- unique(as.character(methods$server))
  missing <- setdiff(expected, inventory_servers)
  unexpected <- setdiff(inventory_servers, expected)
  if (length(missing) > 0L || length(unexpected) > 0L) {
    detail <- c(
      if (length(missing) > 0L) {
        paste0("missing server(s): ", paste(missing, collapse = ", "))
      },
      if (length(unexpected) > 0L) {
        paste0("unexpected server(s): ", paste(unexpected, collapse = ", "))
      }
    )
    stop("Could not verify DataSHIELD AggregateMethods on every connected ",
         "server (", paste(detail, collapse = "; "), ").", call. = FALSE)
  }

  normalize_target <- function(x) {
    x <- gsub("`", "", as.character(x), fixed = TRUE)
    x <- gsub("[[:space:]]+", "", x)
    x <- sub("\\(\\)$", "", x)
    tolower(sub("^base:::", "base::", x))
  }
  method_names <- normalize_target(methods$name)
  values <- normalize_target(methods$value)
  packages <- trimws(as.character(methods$package))
  unsafe <- method_names %in% c("c", "list") |
    values %in% c("c", "list", "base::c", "base::list")
  if (any(unsafe)) {
    offenders <- paste0(
      as.character(methods$server[unsafe]), "/", packages[unsafe], "/",
      trimws(as.character(methods$name[unsafe])), "->",
      as.character(methods$value[unsafe])
    )
    stop("Unsafe DataSHIELD AggregateMethods are enabled (",
         paste(offenders, collapse = ", "), "). Methods named c/list or ",
         "aliased to base::c/base::list can release unreviewed raw objects; ",
         "remove them before connecting.", call. = FALSE)
  }

  invisible(methods)
}

#' Build the common disclosure/harmonisation contract for a federation
#'
#' The contract is deliberately a coarsening contract, not a "first server
#' wins" rule. Age boundaries are intersected across every node, minimum age
#' and date windows take the largest (most restrictive) value, and count bands
#' must be identical because sums of differently rounded site counts do not
#' have one documented release granularity.
#'
#' @param settings Named list of per-server disclosure settings.
#' @param expected_servers Character vector of servers that must be represented.
#' @param fail Logical; stop when no common semantic/age contract exists.
#' @return A named harmonisation-contract list.
#' @keywords internal
.federated_harmonization_contract <- function(settings,
                                               expected_servers = names(settings),
                                               fail = TRUE) {
  expected_servers <- as.character(expected_servers)
  missing_servers <- setdiff(expected_servers, names(settings))
  settings <- settings[intersect(expected_servers, names(settings))]

  field <- function(name, default = NULL) {
    lapply(settings, function(x) x[[name]] %||% default)
  }
  scalar_chars <- function(name) {
    vapply(field(name), function(x) {
      if (length(x) == 1L && !is.na(x)) as.character(x) else NA_character_
    }, character(1))
  }
  scalar_nums <- function(name) {
    vapply(field(name), function(x) {
      value <- suppressWarnings(as.numeric(x))
      if (length(value) == 1L && is.finite(value)) value else NA_real_
    }, numeric(1))
  }

  versions <- scalar_chars("harmonization_contract_version")
  age_semantics <- scalar_chars("age_semantics")
  date_semantics <- scalar_chars("date_semantics")
  date_granularity <- scalar_chars("date_granularity")
  datetime_timezone <- scalar_chars("datetime_timezone")
  week_start <- scalar_chars("week_start")
  age_grids <- lapply(field("age_breaks"), function(x) {
    x <- suppressWarnings(as.integer(unlist(x, use.names = FALSE)))
    if (length(x) < 2L || anyNA(x) || x[1L] != 0L || any(diff(x) <= 0L)) {
      integer(0)
    } else x
  })
  common_age_breaks <- if (length(age_grids) > 0L &&
      all(lengths(age_grids) >= 2L)) {
    sort(Reduce(intersect, age_grids))
  } else integer(0)

  age_grid_ok <- length(common_age_breaks) >= 2L &&
    identical(common_age_breaks[1L], 0L)
  semantic_ok <- length(settings) > 0L && !anyNA(versions) &&
    all(versions == "dsomop-harmonization-v3") &&
    !anyNA(age_semantics) &&
    length(unique(age_semantics)) == 1L && !anyNA(date_semantics) &&
    length(unique(date_semantics)) == 1L && !anyNA(date_granularity) &&
    length(unique(date_granularity)) == 1L && !anyNA(datetime_timezone) &&
    length(unique(datetime_timezone)) == 1L && !anyNA(week_start) &&
    length(unique(week_start)) == 1L
  age_grids_identical <- length(age_grids) > 0L &&
    all(vapply(age_grids[-1L], identical, logical(1), age_grids[[1L]]))

  band_widths <- scalar_nums("nfilter_band")
  count_bands_compatible <- length(band_widths) > 0L &&
    !anyNA(band_widths) && length(unique(band_widths)) == 1L
  age_min <- scalar_nums("nfilter_age_range")
  date_min <- scalar_nums("nfilter_date_range")
  cap_names <- c(
    "max_feature_specs", "max_pivot_concepts", "max_output_columns",
    "max_temporal_bins", "max_events_per_group", "max_filter_depth",
    "max_filter_nodes",
    "max_filter_values", "max_plan_outputs", "max_analysis_scope_tables"
  )
  cap_values <- stats::setNames(lapply(cap_names, scalar_nums), cap_names)
  cap_fields_valid <- vapply(cap_values, function(x) {
    length(x) == length(settings) && length(x) > 0L && !anyNA(x) &&
      all(x >= 1 & x == floor(x))
  }, logical(1))
  resource_caps_compatible <- all(cap_fields_valid)
  negotiated_caps <- vapply(cap_names, function(name) {
    values <- cap_values[[name]]
    if (isTRUE(cap_fields_valid[[name]])) min(values) else NA_real_
  }, numeric(1))

  contract <- list(
    version = if (semantic_ok) unname(versions[[1L]]) else NA_character_,
    age_semantics = if (semantic_ok) unname(age_semantics[[1L]]) else NA_character_,
    date_semantics = if (semantic_ok) unname(date_semantics[[1L]]) else NA_character_,
    date_granularity = if (semantic_ok) unname(date_granularity[[1L]]) else NA_character_,
    datetime_timezone = if (semantic_ok) unname(datetime_timezone[[1L]]) else NA_character_,
    week_start = if (semantic_ok) unname(week_start[[1L]]) else NA_character_,
    servers = expected_servers,
    missing_servers = missing_servers,
    common_age_breaks = common_age_breaks,
    common_age_groups = if (age_grid_ok) c(
      paste0(common_age_breaks[-length(common_age_breaks)], "-",
             common_age_breaks[-1L] - 1L),
      paste0(common_age_breaks[length(common_age_breaks)], "+")) else character(0),
    age_grids_identical = age_grids_identical,
    min_age_range_years = if (length(age_min) > 0L && !anyNA(age_min))
      max(age_min) else NA_real_,
    min_date_range_days = if (length(date_min) > 0L && !anyNA(date_min))
      max(date_min) else NA_real_,
    count_band_width = if (count_bands_compatible)
      unname(band_widths[[1L]]) else NA_real_,
    count_band_widths = band_widths,
    count_bands_compatible = count_bands_compatible,
    max_feature_specs = unname(negotiated_caps[["max_feature_specs"]]),
    max_pivot_concepts = unname(negotiated_caps[["max_pivot_concepts"]]),
    max_output_columns = unname(negotiated_caps[["max_output_columns"]]),
    max_temporal_bins = unname(negotiated_caps[["max_temporal_bins"]]),
    max_events_per_group = unname(
      negotiated_caps[["max_events_per_group"]]
    ),
    max_filter_depth = unname(negotiated_caps[["max_filter_depth"]]),
    max_filter_nodes = unname(negotiated_caps[["max_filter_nodes"]]),
    max_filter_values = unname(negotiated_caps[["max_filter_values"]]),
    max_plan_outputs = unname(negotiated_caps[["max_plan_outputs"]]),
    max_analysis_scope_tables = unname(
      negotiated_caps[["max_analysis_scope_tables"]]
    ),
    resource_caps_compatible = resource_caps_compatible,
    semantic_contract_compatible = semantic_ok,
    age_grid_compatible = age_grid_ok,
    compatible = length(missing_servers) == 0L && semantic_ok && age_grid_ok &&
      resource_caps_compatible,
    poolable_counts = length(missing_servers) == 0L && semantic_ok &&
      resource_caps_compatible && count_bands_compatible
  )

  if (isTRUE(fail) && !isTRUE(contract$compatible)) {
    reason <- if (length(missing_servers) > 0L) {
      paste0("missing disclosure metadata from ",
             paste(missing_servers, collapse = ", "))
    } else if (!semantic_ok) {
      "age/date semantics or contract versions differ"
    } else if (!resource_caps_compatible) {
      "v3 operational caps are missing or invalid"
    } else {
      "age grids have no verifiable common coarsening"
    }
    stop("Federated harmonisation failed: ", reason, ".", call. = FALSE)
  }
  contract
}

.capability_disclosure_settings <- function(capabilities) {
  if (!is.list(capabilities)) return(list())
  out <- lapply(capabilities, function(cap) {
    if (is.list(cap)) cap$disclosure else NULL
  })
  out[!vapply(out, is.null, logical(1))]
}

.session_harmonization_for_connections <- function(session, conns,
                                                    require_count_pooling = FALSE) {
  servers <- names(conns)
  if (length(servers) <= 1L) return(NULL)
  caps <- session$capabilities
  if (is.list(caps)) caps <- caps[intersect(servers, names(caps))]
  contract <- .federated_harmonization_contract(
    .capability_disclosure_settings(caps), expected_servers = servers,
    fail = TRUE)
  if (isTRUE(require_count_pooling) && !isTRUE(contract$poolable_counts)) {
    widths <- contract$count_band_widths
    detail <- if (length(widths) > 0L) {
      paste(names(widths), widths, sep = "=", collapse = ", ")
    } else "metadata unavailable"
    stop("Federated count pooling blocked: server count-band widths are not ",
         "identical (", detail, "). Exact common coarsening is not available; ",
         "align dsomop.nfilter.band across servers.", call. = FALSE)
  }
  contract
}

.pooling_harmonization_for_servers <- function(servers) {
  if (length(servers) <= 1L) return(NULL)
  candidates <- mget(ls(envir = .dsomop_client_env, all.names = TRUE),
                     envir = .dsomop_client_env, ifnotfound = list(NULL))
  candidates <- Filter(function(x) inherits(x, "omop_session") &&
                         all(servers %in% (x$server_names %||% character(0))),
                       candidates)
  if (length(candidates) == 0L) return(NULL)
  if (length(candidates) != 1L) {
    return(list(
      poolable_counts = FALSE,
      count_band_widths = numeric(0),
      compatible = FALSE,
      reason = "ambiguous active OMOP session"
    ))
  }
  session <- candidates[[1L]]
  caps <- session$capabilities[servers]
  .federated_harmonization_contract(
    .capability_disclosure_settings(caps), expected_servers = servers,
    fail = FALSE)
}

#' Inject data-independent federation settings into a plan
#'
#' Baseline age groups are materialized on each server. When local public age
#' grids differ, send their negotiated common coarsening so every site emits
#' the same labels instead of rejecting an otherwise compatible analysis.
#'
#' @param plan Extraction plan.
#' @param contract Compatible federation contract.
#' @return The plan with common age breaks attached where required.
#' @keywords internal
.apply_plan_harmonization <- function(plan, contract) {
  if (!isTRUE(contract$compatible)) return(plan)
  common <- suppressWarnings(as.integer(contract$common_age_breaks))
  if (length(common) < 2L || anyNA(common) || common[1L] != 0L ||
      any(diff(common) <= 0L)) {
    stop("Federated contract has no usable common age grid.", call. = FALSE)
  }
  outputs <- plan$outputs %||% list()
  for (name in names(outputs)) {
    out <- outputs[[name]]
    if (identical(out$type %||% NULL, "baseline") &&
        "age_at_index" %in% unlist(out$derived %||% character(0),
                                    use.names = FALSE)) {
      existing <- out$age_breaks
      if (!is.null(existing)) {
        existing <- suppressWarnings(as.integer(
          unlist(existing, use.names = FALSE)
        ))
        if (!identical(existing, common)) {
          stop("Baseline output '", name, "' has age_breaks that differ from ",
               "the negotiated common federation grid.", call. = FALSE)
        }
      }
      out$age_breaks <- common
      outputs[[name]] <- out
    }
  }
  plan$outputs <- outputs
  plan
}

#' Validate a plan against an already negotiated federation contract
#' @keywords internal
.validate_plan_harmonization <- function(plan, contract) {
  if (!isTRUE(contract$compatible)) {
    stop("Federated harmonisation contract is not compatible.", call. = FALSE)
  }

  cap_names <- c(
    "max_feature_specs", "max_pivot_concepts", "max_output_columns",
    "max_temporal_bins", "max_events_per_group", "max_filter_depth",
    "max_filter_nodes",
    "max_filter_values", "max_plan_outputs", "max_analysis_scope_tables"
  )
  caps <- vapply(cap_names, function(name) {
    value <- suppressWarnings(as.numeric(contract[[name]]))
    if (length(value) != 1L || !is.finite(value) || value < 1 ||
        value != floor(value)) {
      stop("Federated contract has no valid ", name, ".", call. = FALSE)
    }
    value
  }, numeric(1))
  assert_cap <- function(requested, name, path) {
    if (is.finite(requested) && requested > caps[[name]]) {
      stop("Federated plan exceeds negotiated ", name, " cap of ",
           caps[[name]], " at ", path, " (requested ", requested, ").",
           call. = FALSE)
    }
  }

  # Guard the recursive harmonisation walkers themselves. Filter trees are
  # additionally checked per root below, matching the server-owned contract.
  stack <- list(list(value = plan, depth = 1L))
  while (length(stack) > 0L) {
    item <- stack[[length(stack)]]
    stack[[length(stack)]] <- NULL
    if (!is.list(item$value)) next
    assert_cap(item$depth, "max_filter_depth", "plan structure")
    for (child in item$value) {
      if (is.list(child)) {
        stack[[length(stack) + 1L]] <- list(
          value = child, depth = item$depth + 1L
        )
      }
    }
  }
  assert_cap(length(plan$outputs %||% list()), "max_plan_outputs",
             "plan$outputs")
  assert_cap(length(plan$scope$tables %||% character(0)),
             "max_analysis_scope_tables", "plan$scope$tables")

  filter_roots <- list()
  collect_roots <- function(x, named_by = NULL) {
    if (!is.list(x)) return(invisible(NULL))
    node_names <- names(x) %||% character(0)
    is_leaf <- all(c("type", "params") %in% node_names) ||
      all(c("var", "op") %in% node_names)
    is_group <- length(intersect(node_names, c("and", "or"))) > 0L
    if ((!is.null(named_by) &&
         named_by %in% c("filter_tree", "custom")) || is_leaf || is_group) {
      filter_roots[[length(filter_roots) + 1L]] <<- x
      return(invisible(NULL))
    }
    child_names <- names(x)
    for (i in seq_along(x)) {
      name <- if (is.null(child_names)) NULL else child_names[[i]]
      collect_roots(x[[i]], name)
    }
    invisible(NULL)
  }
  collect_roots(plan)
  for (root_index in seq_along(filter_roots)) {
    nodes <- 0L
    values <- 0L
    filter_stack <- list(list(value = filter_roots[[root_index]], depth = 1L,
                              count_values = FALSE))
    while (length(filter_stack) > 0L) {
      item <- filter_stack[[length(filter_stack)]]
      filter_stack[[length(filter_stack)]] <- NULL
      x <- item$value
      if (!is.list(x)) {
        if (isTRUE(item$count_values)) values <- values + length(x)
        next
      }
      node_names <- names(x) %||% character(0)
      is_leaf <- all(c("type", "params") %in% node_names) ||
        all(c("var", "op") %in% node_names)
      group_key <- intersect(node_names, c("and", "or"))
      is_group <- length(group_key) > 0L
      is_flat <- is.null(names(x))
      if (is_leaf || is_group || is_flat) {
        nodes <- nodes + 1L
        assert_cap(nodes, "max_filter_nodes",
                   paste0("filter tree ", root_index))
        assert_cap(item$depth, "max_filter_depth",
                   paste0("filter tree ", root_index))
      }
      if (is_leaf) {
        value_field <- if ("params" %in% node_names) "params" else "value"
        filter_stack[[length(filter_stack) + 1L]] <- list(
          value = x[[value_field]], depth = item$depth,
          count_values = TRUE
        )
      } else if (is_group) {
        children <- x[[group_key[[1L]]]]
        for (child in children) {
          filter_stack[[length(filter_stack) + 1L]] <- list(
            value = child, depth = item$depth + 1L, count_values = FALSE
          )
        }
      } else if (is_flat) {
        for (child in x) {
          filter_stack[[length(filter_stack) + 1L]] <- list(
            value = child, depth = item$depth + 1L,
            count_values = item$count_values
          )
        }
      } else if (isTRUE(item$count_values)) {
        for (child in x) {
          filter_stack[[length(filter_stack) + 1L]] <- list(
            value = child, depth = item$depth, count_values = TRUE
          )
        }
      }
      assert_cap(values, "max_filter_values",
                 paste0("filter tree ", root_index))
    }
    assert_cap(values, "max_filter_values",
               paste0("filter tree ", root_index))
  }

  leaves <- list()
  calendar_windows <- list()
  add_calendar_window <- function(start, end) {
    calendar_windows[[length(calendar_windows) + 1L]] <<- list(
      start = start, end = end)
  }
  is_calendar_value <- function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt")) ||
      (is.character(x) && length(x) == 1L && !is.na(x) &&
         grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x))
  }
  walk_dates <- function(x) {
    if (!is.list(x)) return(invisible(NULL))
    type <- x$type %||% NULL
    if (length(type) == 1L && type %in% c("age_range", "age_group",
                                          "date_range")) {
      leaves[[length(leaves) + 1L]] <<- x
    }
    # start/end is also the shape of an index-relative day window. Only
    # calendar-looking endpoints are governed by nfilter_date_range.
    if (!is.null(x$start) && !is.null(x$end) &&
        is_calendar_value(x$start) && is_calendar_value(x$end)) {
      add_calendar_window(x$start, x$end)
    }
    if (!is.null(x$start_date) && !is.null(x$end_date)) {
      add_calendar_window(x$start_date, x$end_date)
    }
    lapply(x, walk_dates)
    invisible(NULL)
  }
  walk_dates(plan)

  feature_collection <- function(x) {
    is.list(x) && (length(x) == 0L || all(vapply(x, function(spec) {
      is.null(spec) || is.list(spec)
    }, logical(1))))
  }
  explicit_concepts <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.list(x)) {
      for (name in c("ids", "concepts", "concept_id")) {
        if (!is.null(x[[name]])) return(explicit_concepts(x[[name]]))
      }
      if (is.null(names(x)) || all(!nzchar(names(x)))) {
        return(explicit_concepts(unlist(x, use.names = FALSE)))
      }
      return(NULL)
    }
    values <- unlist(x, use.names = FALSE)
    values <- values[!is.na(values)]
    unique(values)
  }
  representation_format <- function(x) {
    representation <- x$representation %||% NULL
    if (is.list(representation)) {
      representation$format %||% NULL
    } else if (is.character(representation) && length(representation) == 1L) {
      representation
    } else {
      x$format %||% NULL
    }
  }
  plan_path <- function(path, name, index) {
    if (!is.null(name) && !is.na(name) && nzchar(name)) {
      paste0(path, "$", name)
    } else {
      paste0(path, "[[", index, "]]")
    }
  }
  walk_caps <- function(x, path = "plan") {
    if (!is.list(x)) return(invisible(NULL))

    for (feature_name in c("features", "feature_specs")) {
      specs <- x[[feature_name]]
      if (!is.null(specs) && feature_collection(specs)) {
        n_specs <- length(specs)
        assert_cap(n_specs, "max_feature_specs",
                   paste0(path, "$", feature_name))
        group_columns <- if (identical(x$grain %||% "person", "episode")) {
          2L
        } else {
          1L
        }
        assert_cap(group_columns + n_specs, "max_output_columns",
                   paste0(path, "$", feature_name))
      }
    }

    type <- x$type %||% NULL
    format <- representation_format(x)
    representation <- if (is.list(x$representation)) x$representation else list()
    concept_spec <- x$filters$concept_set$ids %||% x$concept_set %||% NULL
    concepts <- explicit_concepts(concept_spec)
    n_concepts <- if (is.null(concepts)) 0L else length(concepts)
    if (n_concepts > 0L) {
      assert_cap(n_concepts, "max_filter_values", path)
    }
    declared_features <- representation$features %||% x$features
    has_feature_specs <- !is.null(declared_features) &&
      length(declared_features) > 0L
    is_temporal_output <- identical(type, "temporal_covariates") ||
      identical(type, "person_period")
    pivots_concepts <- is_temporal_output ||
      format %in% c("wide", "sparse") ||
      (identical(format, "features") && !has_feature_specs)
    if (isTRUE(pivots_concepts) && n_concepts > 0L) {
      assert_cap(n_concepts, "max_pivot_concepts", path)
    }

    if (identical(format, "features") && !has_feature_specs &&
        n_concepts > 0L) {
      group_columns <- if (identical(representation$grain %||% x$grain %||%
                                      "person", "episode")) 2L else 1L
      assert_cap(group_columns + 5 * as.double(n_concepts),
                 "max_output_columns", path)
    }

    if (identical(format, "wide") && n_concepts > 0L) {
      columns <- unlist(x$columns %||% character(0), use.names = FALSE)
      columns <- unique(tolower(as.character(columns[!is.na(columns)])))
      concept_col <- tolower(as.character(
        x$filters$concept_col %||% character(0)))
      identifier_col <- grepl("_id$", columns) &
        !grepl("_concept_id$", columns)
      value_columns <- setdiff(columns[!identifier_col], concept_col)
      n_values <- max(1L, length(value_columns))
      group_columns <- if (identical(representation$grain %||% "person",
                                     "episode")) 2L else 1L
      assert_cap(group_columns + as.double(n_concepts) * n_values,
                 "max_output_columns", path)
    }

    if (is_temporal_output) {
      bin_width <- suppressWarnings(as.numeric(x$bin_width))
      window_start <- suppressWarnings(as.numeric(x$window_start))
      window_end <- suppressWarnings(as.numeric(x$window_end))
      if (length(bin_width) == 1L && is.finite(bin_width) && bin_width > 0 &&
          length(window_start) == 1L && is.finite(window_start) &&
          length(window_end) == 1L && is.finite(window_end)) {
        n_bins <- floor((window_end - window_start) / bin_width) + 1
        assert_cap(n_bins, "max_temporal_bins", path)
      }
    }

    if (identical(type, "intervals_long")) {
      select_n <- suppressWarnings(as.numeric(x$select_n %||% 1L))
      if (length(select_n) == 1L && is.finite(select_n)) {
        assert_cap(select_n, "max_events_per_group", path)
      }
    }

    if (identical(type, "person_level")) {
      entries <- x$tables %||% list()
      nested_features <- sum(vapply(entries, function(entry) {
        if (is.list(entry) && feature_collection(entry$features)) {
          length(entry$features)
        } else 0L
      }, integer(1)))
      known_columns <- 1L + nested_features +
        length(x$derived_columns %||% list())
      assert_cap(known_columns, "max_output_columns", path)
    }

    child_names <- names(x)
    for (i in seq_along(x)) {
      child_name <- if (is.null(child_names)) NULL else child_names[[i]]
      walk_caps(x[[i]], plan_path(path, child_name, i))
    }
    invisible(NULL)
  }
  walk_caps(plan)

  on_common_age_grid <- function(groups) {
    breaks <- contract$common_age_breaks
    groups <- unlist(groups, use.names = FALSE)
    length(groups) > 0L && !anyNA(groups) && all(vapply(groups, function(g) {
      g <- trimws(as.character(g))
      if (grepl("^[0-9]+\\+$", g)) {
        return(as.integer(sub("\\+$", "", g)) %in% breaks)
      }
      if (!grepl("^[0-9]+-[0-9]+$", g)) return(FALSE)
      b <- suppressWarnings(as.integer(strsplit(g, "-", fixed = TRUE)[[1L]]))
      length(b) == 2L && !anyNA(b) && b[1L] %in% breaks &&
        (b[2L] + 1L) %in% breaks
    }, logical(1)))
  }

  for (leaf in leaves) {
    params <- leaf$params %||% list()
    if (identical(leaf$type, "age_range") && !is.null(params$min) &&
        !is.null(params$max)) {
      width <- as.numeric(params$max) - as.numeric(params$min) + 1
      if (!is.finite(width) || width < contract$min_age_range_years) {
        stop("Federated age_range is narrower than the common ",
             contract$min_age_range_years, "-year disclosure floor.",
             call. = FALSE)
      }
    }
    if (identical(leaf$type, "age_group") &&
        !on_common_age_grid(params$groups)) {
      stop("Federated age_group is not a union of the negotiated common age ",
           "grid (", paste(contract$common_age_groups, collapse = ", "), ").",
           call. = FALSE)
    }
    if (identical(leaf$type, "date_range") && !is.null(params$start) &&
        !is.null(params$end)) {
      width <- as.numeric(as.Date(params$end) - as.Date(params$start)) + 1
      if (!is.finite(width) || width < contract$min_date_range_days) {
        stop("Federated date_range is narrower than the common ",
             contract$min_date_range_days, "-day disclosure floor.",
             call. = FALSE)
      }
    }
  }

  for (window in calendar_windows) {
    width <- tryCatch(
      as.numeric(as.Date(window$end) - as.Date(window$start)) + 1,
      error = function(e) NA_real_)
    if (length(width) != 1L || !is.finite(width) ||
        width < contract$min_date_range_days) {
      stop("Federated bounded calendar window is narrower than the common ",
           contract$min_date_range_days, "-day disclosure floor.",
           call. = FALSE)
    }
  }

  # Grouped age outputs must carry the common grid injected above; validating
  # it before execution prevents a caller from reintroducing one node's local
  # boundaries after federation negotiation.
  has_binned_age <- FALSE
  age_breaks_are_common <- TRUE
  find_age <- function(x) {
    if (!is.list(x)) return(invisible(NULL))
    # A `kind = "age"` derived variable is the same annual-resolution numeric
    # value on every compatible server and does not use the local age grid.
    # Only grouped age_at_index materialisation depends on those boundaries.
    if ("age_at_index" %in% unlist(x$derived %||% character(0),
                                    use.names = FALSE)) {
      has_binned_age <<- TRUE
      requested <- suppressWarnings(as.integer(
        unlist(x$age_breaks %||% integer(0), use.names = FALSE)
      ))
      age_breaks_are_common <<- age_breaks_are_common &&
        identical(requested, as.integer(contract$common_age_breaks))
    }
    lapply(x, find_age)
    invisible(NULL)
  }
  find_age(plan$outputs)
  if (has_binned_age && !isTRUE(age_breaks_are_common)) {
    stop("Federated age output must use the negotiated common age grid.",
         call. = FALSE)
  }
  invisible(contract)
}

#' Connect to an OMOP CDM resource on DataSHIELD servers
#'
#' Establishes a connection to one or more OMOP CDM databases via DataSHIELD.
#' Assigns the resource server-side, initializes the OMOP handle, retrieves
#' capabilities, and returns an \code{omop_session} object. This is the entry
#' point for all dsOMOPClient operations.
#'
#' @details
#' Connection setup is all-or-none across the requested servers. The public
#' handle symbol must be absent everywhere, one resource must resolve for every
#' node, and capabilities plus transient-resource cleanup are verified before
#' the local session is recorded. Partial initialization is closed and removed.
#' Schema arguments are passed as literal call values, never parsed into code.
#'
#' Before assigning any resource, the client requires a complete AggregateMethods
#' inventory from every node. A method named \code{c}/\code{list}, or an alias
#' targeting \code{base::c}/\code{base::list}, aborts connection because it can
#' wrap and release an unreviewed protected object. This safety preflight is
#' mandatory even when \code{strict = FALSE}.
#'
#' The connection is self-healing: the server-side OMOP database connection
#' auto-reconnects on demand, so a dropped or timed-out database connection is
#' transparently re-established on the next call. There is therefore no need to
#' keep the session warm with periodic pings during long idle periods. Use
#' \code{\link{ds.omop.status}} if you want to manually probe connectivity.
#'
#' @param resource Character or named list; resource name(s). A single string
#'   applies to all servers; a named list maps server names to resource names.
#' @param symbol Character; server-side symbol name (default: "omop").
#' @param cdm_schema Character; CDM schema override (NULL uses server default).
#' @param vocab_schema Character; vocabulary schema override (NULL uses server default).
#' @param results_schema Character; results schema override (NULL uses server default).
#' @param temp_schema Character; temp schema override (NULL uses server default).
#' @param strict Logical; whether unknown/missing names in a named resource map
#'   are immediate mapping errors (\code{TRUE}) or warnings where possible.
#'   Transactional connection still requires a valid resource and successful
#'   initialization on every requested server regardless of this value.
#' @param conns DSI connections object (NULL uses default connections).
#' @return An \code{omop_session} object (invisibly).
#' @examples
#' \dontrun{
#' library(DSI)
#' builder <- newDSLoginBuilder()
#' builder$append(server = "server1", url = "https://opal.example.org",
#'                resource = "project.omop_cdm", driver = "OpalDriver")
#' conns <- datashield.login(builder$build())
#' session <- ds.omop.connect(resource = "project.omop_cdm", conns = conns)
#' }
#' @seealso \code{\link{ds.omop.disconnect}}, \code{\link{ds.omop.status}}
#' @export
ds.omop.connect <- function(resource,
                            symbol = "omop",
                            cdm_schema = NULL,
                            vocab_schema = NULL,
                            results_schema = NULL,
                            temp_schema = NULL,
                            strict = TRUE,
                            conns = NULL) {
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    stop("strict must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(symbol) || length(symbol) != 1L || is.na(symbol) ||
      !grepl("^[A-Za-z][A-Za-z0-9._]*$", symbol)) {
    stop("symbol must be one safe R identifier.", call. = FALSE)
  }
  if (exists(symbol, envir = .dsomop_client_env, inherits = FALSE)) {
    stop("OMOP session '", symbol, "' is already active; disconnect it before ",
         "reusing the symbol.", call. = FALSE)
  }
  conns <- conns %||% DSI::datashield.connections_default()
  if (is.null(conns) || length(conns) == 0L || is.null(names(conns)) ||
      anyNA(names(conns)) || any(!nzchar(names(conns))) ||
      anyDuplicated(names(conns))) {
    stop("No DataSHIELD connections available.", call. = FALSE)
  }

  server_names <- names(conns)
  .assert_safe_aggregate_methods(conns)

  resource_map <- .resolve_resource_map(resource, server_names, strict = strict)
  missing_resources <- server_names[vapply(resource_map, function(x) {
    is.null(x) || length(x) != 1L || is.na(x) || !nzchar(x)
  }, logical(1))]
  if (length(missing_resources) > 0L) {
    stop("Transactional OMOP connection requires one resource on every ",
         "server; missing: ", paste(missing_resources, collapse = ", "), ".",
         call. = FALSE)
  }

  inventory <- .plan_symbol_inventory(conns, "OMOP connection preflight")
  occupied <- names(inventory)[vapply(inventory, function(x) {
    symbol %in% x
  }, logical(1))]
  if (length(occupied) > 0L) {
    stop("Server-side symbol '", symbol, "' already exists on: ",
         paste(occupied, collapse = ", "),
         ". Remove or disconnect it before connecting.", call. = FALSE)
  }

  res_symbol <- .generate_symbol("dsO")

  # Load dsOMOP server-side before assigning the resource. Opal invokes methods
  # as `dsOMOP::fn`, so the namespace (and the resource resolver it registers in
  # .onLoad) is not loaded until the first dsOMOP call. Without this ping, the
  # assign below runs first and fails with "No resolver could be found". A
  # server that cannot answer it (dsOMOP missing, or its methods not published
  # in the profile) stops the connection here, with that server's reason and
  # what to do, before anything is assigned.
  ping <- .ds_safe_aggregate(conns, call("omopPingDS"))
  ping_errors <- attr(ping, "ds_errors") %||% list()
  not_ready <- unique(c(names(ping_errors), setdiff(server_names, names(ping))))
  if (length(not_ready) > 0L) {
    stop("dsOMOP is not ready on ", paste(not_ready, collapse = ", "), ":\n",
         .ds_failure_detail(ping_errors, servers = not_ready), call. = FALSE)
  }

  remove_symbol <- function(servers, target) {
    failures <- character(0)
    for (server in intersect(server_names, servers)) {
      tryCatch(
        DSI::datashield.rm(conns[server], target),
        error = function(e) {
          failures[[server]] <<- conditionMessage(e)
        }
      )
    }
    failures
  }
  close_handles <- function(servers) {
    servers <- intersect(server_names, servers)
    if (length(servers) == 0L) return(character(0))
    close_symbol <- .generate_symbol("dsOclose")
    succeeded <- character(0)
    failures <- character(0)
    condition <- tryCatch({
      DSI::datashield.assign.expr(
        conns[servers], symbol = close_symbol,
        expr = call("omopCleanupDS", res_symbol, close = TRUE),
        success = function(server) {
          succeeded <<- c(succeeded, server)
        },
        error = function(server, message) {
          failures[[server]] <<- message
        }
      )
      NULL
    }, error = identity)
    close_symbol_errors <- remove_symbol(servers, close_symbol)
    missing <- setdiff(servers, succeeded)
    if (!is.null(condition)) {
      failures[["transport"]] <- conditionMessage(condition)
    }
    for (server in missing) {
      if (is.null(failures[[server]])) failures[[server]] <- "no success callback"
    }
    c(failures, close_symbol_errors)
  }
  rollback <- function(initialized, resources) {
    close_errors <- close_handles(initialized)
    public_errors <- remove_symbol(resources, symbol)
    resource_errors <- remove_symbol(resources, res_symbol)
    c(close_errors, public_errors, resource_errors)
  }
  fail_transaction <- function(message, initialized, resources) {
    cleanup_errors <- rollback(initialized, resources)
    suffix <- if (length(cleanup_errors) > 0L) {
      paste0(" Cleanup could not be proven on: ",
             paste(unique(names(cleanup_errors)), collapse = ", "), ".")
    } else ""
    stop(message, suffix, call. = FALSE)
  }

  resource_success <- character(0)
  errors <- character(0)
  resource_condition <- tryCatch({
    DSI::datashield.assign.resource(
      conns, symbol = res_symbol, resource = resource_map,
      success = function(server) {
        resource_success <<- c(resource_success, server)
      },
      error = function(server, message) {
        errors[[server]] <<- message
      }
    )
    NULL
  }, error = identity)
  failed_resources <- unique(c(names(errors),
                               setdiff(server_names, resource_success)))
  if (!is.null(resource_condition) || length(failed_resources) > 0L) {
    fail_transaction(
      paste0("Failed to assign every OMOP resource:\n",
             .ds_failure_detail(errors, resource_condition, failed_resources)),
      initialized = character(0), resources = resource_success
    )
  }

  # Build a language object with literal arguments. Never parse a string made
  # from schema input: even allowlisted DataSHIELD methods must not be nestable
  # through a crafted schema value.
  init_call <- call(
    "omopInitDS", res_symbol,
    cdm_schema = cdm_schema, vocab_schema = vocab_schema,
    results_schema = results_schema, temp_schema = temp_schema
  )
  init_success <- character(0)
  init_errors <- character(0)
  init_condition <- tryCatch({
    DSI::datashield.assign.expr(
      conns, symbol = symbol, expr = init_call,
      success = function(server) {
        init_success <<- c(init_success, server)
      },
      error = function(server, message) {
        init_errors[[server]] <<- message
      }
    )
    NULL
  }, error = identity)
  failed_init <- unique(c(names(init_errors),
                          setdiff(server_names, init_success)))
  if (!is.null(init_condition) || length(failed_init) > 0L) {
    fail_transaction(
      paste0("Failed to initialize every OMOP handle:\n",
             .ds_failure_detail(init_errors, init_condition, failed_init)),
      initialized = init_success, resources = resource_success
    )
  }

  caps <- .ds_safe_aggregate(
    conns, expr = call("omopGetCapabilitiesDS", res_symbol)
  )
  cap_errors <- attr(caps, "ds_errors") %||% list()
  missing_caps <- setdiff(server_names, names(caps))
  if (length(cap_errors) > 0L || length(missing_caps) > 0L) {
    fail_transaction(
      paste0("Failed to retrieve complete OMOP capabilities:\n",
             .ds_failure_detail(
               cap_errors, servers = unique(c(names(cap_errors), missing_caps))
             )),
      initialized = init_success, resources = resource_success
    )
  }

  harmonization <- .federated_harmonization_contract(
    .capability_disclosure_settings(caps),
    expected_servers = server_names,
    fail = FALSE)

  resource_cleanup_errors <- remove_symbol(resource_success, res_symbol)
  if (length(resource_cleanup_errors) > 0L) {
    fail_transaction(
      paste0("Failed to remove temporary resource symbol on: ",
             paste(names(resource_cleanup_errors), collapse = ", "), "."),
      initialized = init_success, resources = resource_success
    )
  }
  committed_inventory <- .plan_symbol_inventory(
    conns, "OMOP connection commit"
  )
  bad_commit <- names(committed_inventory)[vapply(committed_inventory,
    function(x) !symbol %in% x || res_symbol %in% x, logical(1))]
  if (length(bad_commit) > 0L) {
    fail_transaction(
      paste0("Could not prove OMOP connection commit on: ",
             paste(bad_commit, collapse = ", "), "."),
      initialized = init_success, resources = resource_success
    )
  }

  session <- list(
    symbol = symbol,
    res_symbol = res_symbol,
    resource_map = resource_map,
    conns = conns,
    capabilities = caps,
    harmonization = harmonization,
    server_names = server_names,
    errors = errors
  )
  class(session) <- "omop_session"

  assign(symbol, session, envir = .dsomop_client_env)

  invisible(session)
}

#' Log in and open an OMOP CDM session in one call
#'
#' One-line entry point for first-time users: builds the DataSHIELD login
#' (\code{\link[DSI]{newDSLoginBuilder}} + \code{\link[DSI]{datashield.login}}),
#' then assigns + initialises the OMOP resource (\code{\link{ds.omop.connect}}),
#' returning BOTH the live connections and the OMOP session. It is a thin
#' convenience over the existing lower-level path — \code{datashield.login()}
#' followed by \code{ds.omop.connect()} — with one ownership guarantee: if OMOP
#' initialisation fails after authentication, it logs out those new DataSHIELD
#' connections before propagating the error. Reach for the two-step path when
#' you need a custom login builder, multiple resources per server, or to reuse
#' connections across several OMOP sessions.
#'
#' The single-server common case is one call:
#' \code{ds.omop.login(url, user, password, resource)}. For several servers pass
#' \code{server}/\code{url}/\code{resource} (and, if they differ,
#' \code{user}/\code{password}) as equal-length vectors; scalars are recycled.
#'
#' @param url Character; server URL(s). A single URL or one per server.
#' @param user Character; username(s) (recycled if scalar). Ignored for a
#'   server whose \code{token} is supplied.
#' @param password Character; password(s) (recycled if scalar). Ignored for a
#'   server whose \code{token} is supplied.
#' @param resource Character; the OMOP CDM resource path(s) (e.g.
#'   \code{"project.omop_cdm"}). A single value applies to every server; a named
#'   vector maps server name to resource; an unnamed vector matches positionally.
#' @param server Character; server name(s) (default \code{"server1"}, or
#'   \code{server1..N} when several URLs are given).
#' @param driver Character; the DSI driver to connect with (default
#'   \code{"OpalDriver"}, from the \pkg{DSOpal} package). Recycled if scalar.
#' @param token Character or \code{NULL}; personal access token(s) used instead
#'   of \code{user}/\code{password} where supplied.
#' @param profile Character or \code{NULL}; Opal/Armadillo R server profile(s).
#' @param symbol Character; server-side OMOP session symbol (default
#'   \code{"omop"}).
#' @param ... Further arguments forwarded to \code{\link{ds.omop.connect}}
#'   (e.g. \code{cdm_schema}, \code{strict}).
#' @return Invisibly, a list with \code{conns} (the DSI connections) and
#'   \code{session} (the \code{omop_session}). The session is also stored under
#'   \code{symbol} so every other \code{ds.omop.*} call can default to it.
#' @examples
#' \dontrun{
#' # The whole connect, in one line:
#' login <- ds.omop.login(
#'   url = "https://opal.example.org",
#'   user = "analyst", password = "secret",
#'   resource = "project.omop_cdm")
#' login$conns     # the DataSHIELD connections
#' login$session   # the OMOP session
#' }
#' @seealso \code{\link{ds.omop.connect}}, \code{\link{ds.omop.disconnect}}
#' @export
ds.omop.login <- function(url, user = "", password = "", resource,
                          server = NULL, driver = "OpalDriver",
                          token = NULL, profile = NULL,
                          symbol = "omop", ...) {
  if (missing(url) || length(url) == 0)
    stop("ds.omop.login() needs at least one server 'url'.", call. = FALSE)
  if (missing(resource) || length(resource) == 0)
    stop("ds.omop.login() needs a 'resource'.", call. = FALSE)

  n <- length(url)
  server <- server %||% (if (n == 1) "server1" else paste0("server", seq_len(n)))
  # Recycle scalar credentials/driver across servers so the common single-value
  # form works for several servers without repeating them.
  rec <- function(x) if (length(x) == 1) rep(x, n) else x
  user     <- rec(user)
  password <- rec(password)
  driver   <- rec(driver)
  token    <- if (is.null(token)) rep("", n) else rec(token)
  profile  <- if (is.null(profile)) rep("", n) else rec(profile)
  if (length(server) != n)
    stop("'server' and 'url' must have the same length.", call. = FALSE)

  builder <- DSI::newDSLoginBuilder(.silent = TRUE)
  for (i in seq_len(n)) {
    builder$append(server = server[i], url = url[i], driver = driver[i],
                   user = user[i], password = password[i],
                   token = token[i], profile = profile[i])
  }
  conns <- DSI::datashield.login(builder$build())

  session <- tryCatch(
    ds.omop.connect(resource = resource, symbol = symbol,
                    conns = conns, ...),
    error = function(connect_error) {
      logout_error <- tryCatch({
        DSI::datashield.logout(conns)
        NULL
      }, error = identity)
      if (!is.null(logout_error)) {
        stop(
          "OMOP connection failed and the authenticated DataSHIELD sessions ",
          "could not be logged out. Connection error: ",
          conditionMessage(connect_error), "; logout error: ",
          conditionMessage(logout_error), ".",
          call. = FALSE
        )
      }
      stop(connect_error)
    }
  )

  invisible(list(conns = conns, session = session))
}

.session_disconnect_connections <- function(session, conns) {
  stored <- session$conns
  stored_names <- names(stored)
  supplied_names <- names(conns)
  valid_names <- function(x) {
    !is.null(x) && length(x) > 0L && !anyNA(x) && all(nzchar(x)) &&
      !anyDuplicated(x)
  }
  if (!valid_names(stored_names) || !valid_names(supplied_names) ||
      !setequal(stored_names, supplied_names)) {
    stop("ds.omop.disconnect() must use exactly the servers stored in the ",
         "OMOP session; omit conns to use them safely.", call. = FALSE)
  }
  different <- stored_names[!vapply(stored_names, function(server) {
    identical(stored[[server]], conns[[server]])
  }, logical(1))]
  if (length(different) > 0L) {
    stop("ds.omop.disconnect() received different connection object(s) for: ",
         paste(different, collapse = ", "),
         ". Omit conns to use the session's original connections.",
         call. = FALSE)
  }
  stored
}

#' Disconnect an OMOP session
#'
#' Closes each server-side database handle, removes its temporary/staged
#' artifacts and then removes the public OMOP symbol. Every participating node
#' must report success and symbol removal is verified. On failure the local
#' session registry is retained so cleanup can be retried.
#'
#' @param symbol Character; the session symbol to disconnect (default: "omop").
#' @param conns DSI connections. \code{NULL} uses the session's stored
#'   connections. When supplied, it must contain exactly the same named
#'   connection objects; subsets and replacements are rejected before cleanup.
#' @return Invisible TRUE on success.
#' @examples
#' \dontrun{
#' ds.omop.disconnect("omop")
#' }
#' @seealso \code{\link{ds.omop.connect}}
#' @export
ds.omop.disconnect <- function(symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- if (is.null(conns)) {
    session$conns
  } else {
    .session_disconnect_connections(session, conns)
  }

  expected_servers <- names(conns)
  inventory <- .plan_symbol_inventory(conns, "disconnect preflight")
  close_symbol <- .fresh_symbol_from_inventory(
    inventory, "dsOclose", "disconnect preflight"
  )
  close_success <- character(0)
  close_errors <- character(0)
  close_condition <- tryCatch({
    DSI::datashield.assign.expr(
      conns,
      symbol = close_symbol,
      expr = call("omopCleanupDS", session$res_symbol, close = TRUE),
      success = function(server) {
        close_success <<- c(close_success, server)
      },
      error = function(server, message) {
        close_errors[[server]] <<- message
      }
    )
    NULL
  }, error = identity)

  # The close result contains only TRUE; remove it even if another node failed.
  for (server in expected_servers) {
    tryCatch(DSI::datashield.rm(conns[server], close_symbol),
             error = function(e) NULL)
  }
  failed <- unique(c(names(close_errors),
                     setdiff(expected_servers, close_success)))
  if (!is.null(close_condition) || length(failed) > 0L) {
    detail <- if (!is.null(close_condition)) {
      conditionMessage(close_condition)
    } else paste(failed, collapse = ", ")
    stop("Could not prove OMOP handle cleanup on every server; the client ",
         "session was retained for retry. Affected server(s): ", detail, ".",
         call. = FALSE)
  }

  # Removal is retriable: a previous attempt may already have removed the
  # public symbol on a subset of nodes.  Treat verified absence as success and
  # let the final inventory, rather than an implementation-specific rm error on
  # an absent symbol, decide whether cleanup completed.
  before_remove <- .plan_symbol_inventory(conns, "disconnect symbol cleanup")
  remove_errors <- character(0)
  for (server in expected_servers) {
    if (!symbol %in% before_remove[[server]]) next
    tryCatch(
      DSI::datashield.rm(conns[server], symbol),
      error = function(e) {
        remove_errors[[server]] <<- conditionMessage(e)
        NULL
      }
    )
  }
  inventory <- .plan_symbol_inventory(conns, "disconnect cleanup")
  lingering <- names(inventory)[vapply(inventory, function(x) {
    symbol %in% x || close_symbol %in% x
  }, logical(1))]
  if (length(lingering) > 0L) {
    detail <- remove_errors[intersect(names(remove_errors), lingering)]
    suffix <- if (length(detail) > 0L) {
      paste0(" (", paste(names(detail), detail, sep = ": ", collapse = "; "),
             ")")
    } else ""
    stop("Could not prove OMOP symbol removal on: ",
         paste(lingering, collapse = ", "), suffix,
         "; the client session was retained for retry.", call. = FALSE)
  }

  if (exists(symbol, envir = .dsomop_client_env)) {
    rm(list = symbol, envir = .dsomop_client_env)
  }

  invisible(TRUE)
}

#' Get OMOP session status
#'
#' Pings each connected server and returns the current session status
#' including capabilities, server versions, and any connection errors.
#'
#' @param symbol Character; session symbol (default: "omop").
#' @return Named list with symbol, servers, capabilities, ping results,
#'   and errors.
#' @examples
#' \dontrun{
#' status <- ds.omop.status("omop")
#' status$ping
#' }
#' @export
ds.omop.status <- function(symbol = "omop") {
  session <- .get_session(symbol)

  ping <- tryCatch(
    DSI::datashield.aggregate(
      session$conns,
      expr = call("omopPingDS")
    ),
    error = function(e) list(error = e$message)
  )

  list(
    symbol = symbol,
    servers = session$server_names,
    capabilities = session$capabilities,
    harmonization = session$harmonization,
    ping = ping,
    errors = session$errors
  )
}

#' Inspect the active disclosure thresholds on each server
#'
#' @description
#' Reports the disclosure-control thresholds currently in effect on every
#' connected server, so an analyst or data controller can see the effective
#' floor each server enforces — most importantly \code{nfilter_subset}, the
#' minimum number of distinct persons the per-patient gate
#' (\code{.assertMinPersons}) requires before any result is returned.
#'
#' This is strictly \strong{read-only}. The thresholds are configured
#' server-side through R options (Opal admin panel, Armadillo config, or
#' \code{Rprofile.site}); there is deliberately no client-side way to lower
#' them. Servers may report different floors, so the result is per-server.
#'
#' @param symbol Character; the session symbol (default: \code{"omop"}).
#' @param conns DSI connection object(s) or \code{NULL} to use the session
#'   default.
#' @return A named list, one element per server, each holding that server's
#'   active disclosure settings (e.g. \code{nfilter_subset},
#'   \code{nfilter_tab}, \code{nfilter_levels_max}). Servers that fail to
#'   respond are omitted and their errors attached as a \code{ds_errors}
#'   attribute.
#' @examples
#' \dontrun{
#' settings <- ds.omop.disclosure.settings()
#' # Effective per-patient floor on each server:
#' lapply(settings, function(s) s$nfilter_subset)
#' }
#' @seealso \code{\link{ds.omop.status}}
#' @export
ds.omop.disclosure.settings <- function(symbol = "omop", conns = NULL) {
  session <- .get_session(symbol)
  conns <- conns %||% session$conns

  .ds_safe_aggregate(
    conns,
    expr = call("omopDisclosureSettingsDS")
  )
}

#' Resolve a resource argument into a per-server resource map
#'
#' Accepts a single resource name (applied to every server), a named
#' list/vector mapping server name to resource (servers may hold the OMOP
#' resource at different locations), or an unnamed vector matched positionally
#' to the connected servers. Validates names against the connected servers.
#'
#' @param resource Character scalar, named list/vector, or positional vector.
#' @param server_names Character; names of the connected servers.
#' @param strict Logical; error (vs warn) on unknown/missing server mappings.
#' @return Named list of resource names, one per server (NULL for unmapped).
#' @keywords internal
.resolve_resource_map <- function(resource, server_names, strict = TRUE) {
  # Single shared resource on every server.
  if (is.character(resource) && length(resource) == 1 &&
      is.null(names(resource))) {
    return(stats::setNames(
      as.list(rep(resource, length(server_names))), server_names))
  }
  nm <- names(resource)
  if (!is.null(nm) && all(nzchar(nm))) {
    # Named mapping (list or vector): match by server name.
    unknown <- setdiff(nm, server_names)
    if (length(unknown) > 0) {
      msg <- paste0(
        "resource names not among connected servers (",
        paste(server_names, collapse = ", "), "): ",
        paste(unknown, collapse = ", "))
      if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
    }
    missing <- setdiff(server_names, nm)
    if (length(missing) > 0 && strict) {
      stop("no resource specified for server(s): ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
    return(stats::setNames(lapply(server_names, function(s) {
      v <- resource[[s]]
      if (is.null(v)) NULL else as.character(v)
    }), server_names))
  }
  # Unnamed vector/list: match positionally to the servers.
  vals <- as.character(unlist(resource, use.names = FALSE))
  if (length(vals) != length(server_names)) {
    stop("resource has ", length(vals), " entries but there are ",
         length(server_names), " connected server(s); supply a named ",
         "list/vector (server = resource) or one value per server.",
         call. = FALSE)
  }
  stats::setNames(as.list(vals), server_names)
}
