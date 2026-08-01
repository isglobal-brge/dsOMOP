# Dedicated person-level differential-privacy aggregates.
#
# These methods intentionally do not reuse the ordinary disclosure gate.  A
# branch on an exact count before adding noise would itself disclose data.  The
# only accepted input is a server-created `omop.table`; recipes and assign
# verbs therefore remain the flexible way to select, join and format the
# longitudinal data before a bounded DP statistic is requested.

.DSOMOP_DP_STATISTICS <- c(
  "count", "categorical_histogram", "numeric_histogram",
  "bounded_mean", "binary_rate"
)

.dsomopDpQueryLibraryStatus <- function() {
  path <- system.file(
    "queries", "dp_redesign_registry.json", package = "dsOMOP"
  )
  if (!nzchar(path) || !file.exists(path)) {
    return(list(available = FALSE, literal_sql_authorized = FALSE))
  }
  registry <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  redesigns <- registry$redesigns %||% list()
  valid <- is.list(registry) && identical(as.numeric(registry$schema_version), 1) &&
    is.list(registry$source) && is.list(redesigns) &&
    all(vapply(redesigns, function(entry) {
      is.list(entry) &&
        identical(entry$status, "mapped_to_bounded_sticky_primitive") &&
        is.character(entry$upstream_id) && length(entry$upstream_id) == 1L &&
        is.character(entry$family) && length(entry$family) == 1L
    }, logical(1L)))
  if (!isTRUE(valid)) {
    stop("The installed OHDSI DP redesign registry is malformed.",
         call. = FALSE)
  }
  list(
    available = TRUE,
    upstream_commit = registry$source$commit,
    mapped_to_bounded_sticky_primitive = vapply(
      redesigns, `[[`, character(1L), "upstream_id"
    ),
    primitive_family = vapply(redesigns, `[[`, character(1L), "family"),
    literal_sql_authorized = FALSE,
    formal_dp_certified = FALSE
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

.dsomopDpValueType <- function(value) {
  if (inherits(value, "Date")) return("date")
  if (inherits(value, c("POSIXct", "POSIXlt"))) return("datetime_utc")
  if (is.factor(value)) return("factor")
  if (is.logical(value)) return("logical")
  if (is.integer(value)) return("integer")
  if (is.numeric(value)) return("numeric")
  if (is.character(value)) return("character")
  stop("The selected DP value column has an unsupported type.", call. = FALSE)
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
  usable <- indices[!is.na(values[indices])]
  if (length(usable) == 0L) return(integer(0))
  if (is.null(order_value)) {
    stop("Longitudinal first/last reducers require a public order_by column.",
         call. = FALSE)
  }
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

.dsomopDpCategorical <- function(input, spec, policy, common) {
  variable <- common$variable
  values <- input$x[[variable]]
  .dsomopDpValueType(values)
  value_type <- "categorical_utf8_v1"
  values <- enc2utf8(as.character(values))
  levels <- .dsomopDpSpecAtomic(spec$levels, "levels")
  if (length(levels) < 1L || length(levels) > policy$max_levels) {
    stop("Categorical DP levels exceed the server-owned level cap.",
         call. = FALSE)
  }
  levels <- enc2utf8(as.character(levels))
  if (any(!nzchar(levels)) || any(nchar(levels, type = "bytes") > 256L) ||
      anyDuplicated(levels)) {
    stop("Categorical DP levels must be unique, non-empty public labels.",
         call. = FALSE)
  }
  levels <- sort(levels, method = "radix")
  reducer <- common$reducer
  if (!reducer %in% c("presence", "mode", "first", "last", "any")) {
    stop("Unsupported categorical longitudinal reducer.", call. = FALSE)
  }
  if (identical(reducer, "any")) reducer <- "presence"
  if (!is.null(common$order_by) && !reducer %in% c("first", "last")) {
    stop("Categorical order_by is only valid for first/last reducers.",
         call. = FALSE)
  }
  cap <- if (reducer == "presence") common$max_contributions else 1L
  groups <- .dsomopDpOrderedGroups(input$token)
  selected_cell <- unlist(lapply(groups, function(indices) {
    candidate <- values[indices]
    if (reducer == "presence") {
      candidate <- sort(unique(candidate[!is.na(candidate) &
                                         candidate %in% levels]),
                        method = "radix")
      candidate <- utils::head(candidate, cap)
    } else if (reducer == "mode") {
      candidate <- candidate[!is.na(candidate) & candidate %in% levels]
      if (length(candidate)) {
        frequencies <- table(candidate)
        candidate <- sort(
          names(frequencies)[frequencies == max(frequencies)],
          method = "radix"
        )[[1L]]
      }
    } else {
      index <- .dsomopDpPickIndex(
        indices, values, common$order_value, reducer == "last"
      )
      candidate <- if (length(index)) values[[index]] else character(0)
      candidate <- candidate[candidate %in% levels]
    }
    if (length(candidate)) match(candidate, levels) else integer(0)
  }), use.names = FALSE)
  counts <- tabulate(selected_cell, nbins = length(levels))
  snapshot <- list(counts = as.numeric(counts))
  semantic <- list(
    statistic = "categorical_histogram", variable = variable,
    value_type = value_type, levels = levels, reducer = reducer,
    max_contributions = cap, order_by = common$order_by
  )
  payload_fn <- function(epsilon, policy, release_context, degraded = FALSE) {
    noisy <- if (degraded) rep.int(0, length(counts)) else vapply(
      seq_along(counts), function(index) .dsomopDpNoisyInteger(
        counts[[index]], policy, release_context,
        sprintf("histogram-cell-%08d", index), epsilon, cap
      ), numeric(1L)
    )
    list(
      statistic = "categorical_histogram", levels = levels,
      counts = as.numeric(noisy), reducer = reducer,
      max_contributions = cap, value_type = value_type,
      degraded = isTRUE(degraded)
    )
  }
  list(semantic = semantic, snapshot = snapshot,
       sensitivity = list(l1 = cap, unit = "person"), payload_fn = payload_fn)
}

.dsomopDpNumericHistogram <- function(input, spec, policy, common) {
  variable <- common$variable
  numeric <- .dsomopDpNumericValues(input$x[[variable]])
  breaks <- .dsomopDpBreaks(spec$breaks, numeric$type, policy)
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
  selected <- numeric(0)
  if (reducer == "records") {
    groups <- .dsomopDpOrderedGroups(input$token)
    selected <- as.numeric(unlist(lapply(groups, function(indices) {
      indices <- indices[!is.na(numeric$value[indices])]
      if (!is.null(common$order_value)) {
        indices <- indices[!is.na(common$order_value[indices])]
        indices <- indices[order(common$order_value[indices],
                                 numeric$value[indices], method = "radix")]
      } else {
        indices <- indices[order(numeric$value[indices], method = "radix")]
      }
      indices <- utils::head(indices, common$max_contributions)
      numeric$value[indices]
    }), use.names = FALSE))
    cap <- common$max_contributions
  } else {
    reduced <- .dsomopDpReduceOne(
      input$token, numeric$value, reducer, common$order_value
    )
    selected <- as.numeric(reduced$value)
    cap <- 1L
  }
  selected <- pmin(breaks$numeric[[length(breaks$numeric)]],
                   pmax(breaks$numeric[[1L]], selected))
  cell <- findInterval(selected, breaks$numeric, rightmost.closed = TRUE,
                       all.inside = TRUE)
  counts <- tabulate(cell, nbins = length(breaks$numeric) - 1L)
  semantic <- list(
    statistic = "numeric_histogram", variable = variable,
    value_type = numeric$type, breaks = breaks$public, reducer = reducer,
    max_contributions = cap, order_by = common$order_by,
    interval_contract = "left_closed_right_open_last_closed"
  )
  snapshot <- list(counts = as.numeric(counts))
  payload_fn <- function(epsilon, policy, release_context, degraded = FALSE) {
    noisy <- if (degraded) rep.int(0, length(counts)) else vapply(
      seq_along(counts), function(index) .dsomopDpNoisyInteger(
        counts[[index]], policy, release_context,
        sprintf("histogram-cell-%08d", index), epsilon, cap
      ), numeric(1L)
    )
    list(
      statistic = "numeric_histogram", breaks = breaks$public,
      counts = as.numeric(noisy), reducer = reducer,
      max_contributions = cap, value_type = numeric$type,
      interval_contract = "left_closed_right_open_last_closed",
      degraded = isTRUE(degraded)
    )
  }
  list(semantic = semantic, snapshot = snapshot,
       sensitivity = list(l1 = cap, unit = "person"), payload_fn = payload_fn)
}

.dsomopDpBoundedMean <- function(input, spec, policy, common) {
  variable <- common$variable
  numeric <- .dsomopDpNumericValues(input$x[[variable]])
  if (!identical(numeric$type, "number")) {
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
  reduced <- .dsomopDpReduceOne(
    input$token, numeric$value, reducer, common$order_value
  )
  clipped <- pmin(upper, pmax(lower, as.numeric(reduced$value)))
  grid <- policy$numeric_grid
  quantized <- round((clipped - lower) / span * grid)
  if (anyNA(quantized) || any(!is.finite(quantized))) {
    stop("The bounded mean could not be represented on its public grid.",
         call. = FALSE)
  }
  true_count <- length(quantized)
  # Saturating an integer sufficient statistic is post-processing and remains
  # grid-sensitive by at most one person's contribution. It also avoids a
  # data-dependent overflow error becoming an observable release side channel.
  true_sum <- min(sum(quantized), 2^53 - 1)
  semantic <- list(
    statistic = "bounded_mean", variable = variable,
    value_type = numeric$type, lower = lower, upper = upper,
    reducer = reducer, order_by = common$order_by, numeric_grid = grid
  )
  snapshot <- list(
    count = as.numeric(true_count), sum_grid = as.numeric(true_sum)
  )
  payload_fn <- function(epsilon, policy, release_context, degraded = FALSE) {
    if (degraded) {
      noisy_count <- 0
      noisy_sum <- 0
    } else {
      noisy_count <- .dsomopDpNoisyInteger(
        true_count, policy, release_context, "mean-count", epsilon / 2, 1
      )
      noisy_sum <- .dsomopDpNoisyInteger(
        true_sum, policy, release_context, "mean-sum-grid", epsilon / 2, grid
      )
      noisy_sum <- min(noisy_sum, noisy_count * grid)
    }
    estimate <- if (noisy_count > 0) {
      lower + span * noisy_sum / (noisy_count * grid)
    } else NULL
    list(
      statistic = "bounded_mean", noisy_count = noisy_count,
      noisy_sum_grid = noisy_sum, value = estimate, lower = lower,
      upper = upper, numeric_grid = grid, reducer = reducer,
      value_type = numeric$type,
      degraded = isTRUE(degraded)
    )
  }
  list(
    semantic = semantic, snapshot = snapshot,
    sensitivity = list(count = 1, sum_grid = grid,
                       allocation = "sequential_half_epsilon", unit = "person"),
    payload_fn = payload_fn
  )
}

.dsomopDpBinaryRate <- function(input, spec, policy, common) {
  variable <- common$variable
  values <- input$x[[variable]]
  .dsomopDpValueType(values)
  value_type <- "categorical_utf8_v1"
  values <- enc2utf8(as.character(values))
  positive <- .dsomopDpSpecAtomic(spec$positive, "positive")
  positive <- sort(unique(enc2utf8(as.character(positive))), method = "radix")
  if (length(positive) < 1L || length(positive) > policy$max_levels ||
      any(!nzchar(positive)) ||
      any(nchar(positive, type = "bytes") > 256L)) {
    stop("binary_rate positive values must be non-empty and within the ",
         "server-owned level cap.",
         call. = FALSE)
  }
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
  groups <- .dsomopDpOrderedGroups(input$token)
  binary <- as.logical(unlist(lapply(groups, function(indices) {
    observed <- values[indices]
    nonmissing <- !is.na(observed)
    if (denominator_contract == "nonmissing" && !any(nonmissing)) {
      return(logical(0))
    }
    observed <- observed[nonmissing]
    hit <- if (length(observed) == 0L) {
      FALSE
    } else if (reducer == "any") {
      any(observed %in% positive)
    } else if (reducer == "all") {
      all(observed %in% positive)
    } else {
      index <- .dsomopDpPickIndex(
        indices, values, common$order_value, reducer == "last"
      )
      length(index) == 1L && values[[index]] %in% positive
    }
    hit
  }), use.names = FALSE))
  numerator <- sum(binary)
  denominator <- length(binary)
  semantic <- list(
    statistic = "binary_rate", variable = variable,
    value_type = value_type, positive = positive, reducer = reducer,
    denominator = denominator_contract, order_by = common$order_by
  )
  snapshot <- list(
    numerator = as.numeric(numerator), denominator = as.numeric(denominator)
  )
  payload_fn <- function(epsilon, policy, release_context, degraded = FALSE) {
    if (degraded) {
      noisy_denominator <- 0
      noisy_numerator <- 0
    } else {
      noisy_denominator <- .dsomopDpNoisyInteger(
        denominator, policy, release_context, "rate-denominator",
        epsilon / 2, 1
      )
      noisy_numerator <- .dsomopDpNoisyInteger(
        numerator, policy, release_context, "rate-numerator",
        epsilon / 2, 1
      )
      noisy_numerator <- min(noisy_numerator, noisy_denominator)
    }
    estimate <- if (noisy_denominator > 0) {
      noisy_numerator / noisy_denominator
    } else NULL
    list(
      statistic = "binary_rate", noisy_numerator = noisy_numerator,
      noisy_denominator = noisy_denominator, value = estimate,
      reducer = reducer, denominator = denominator_contract,
      value_type = value_type,
      degraded = isTRUE(degraded)
    )
  }
  list(
    semantic = semantic, snapshot = snapshot,
    sensitivity = list(numerator = 1, denominator = 1,
                       allocation = "sequential_half_epsilon", unit = "person"),
    payload_fn = payload_fn
  )
}

.dsomopDpAnalysis <- function(x, spec, policy) {
  input <- .dsomopDpValidateInput(x, policy)
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
        !grepl("^[A-Za-z0-9][A-Za-z0-9._:@+/-]{0,255}$", population_id)) {
      stop("DP population_id must be one valid public compatibility label.",
           call. = FALSE)
    }
  }
  variable <- if (statistic == "count") NULL else {
    .dsomopDpColumn(input$x, spec$variable, "variable")
  }
  reducer <- tolower(as.character(.dsomopDpSpecScalar(
    spec$reducer %||% "any", "reducer"
  )))
  max_contributions <- .dsomopDpSpecInteger(
    spec$max_contributions %||% 1L, "max_contributions", 1,
    policy$max_contributions
  )
  order_by <- .dsomopDpColumn(
    input$x, spec$order_by, "order_by", required = FALSE
  )
  common <- list(
    variable = variable, reducer = reducer,
    max_contributions = max_contributions, order_by = order_by,
    order_value = .dsomopDpOrderVector(input$x, order_by)
  )
  if (statistic == "count") {
    people <- sort(unique(input$token), method = "radix")
    true_count <- length(people)
    analysis <- list(
      semantic = list(statistic = "count", unit = "distinct_person"),
      snapshot = list(count = as.numeric(true_count)),
      sensitivity = list(l1 = 1, unit = "person"),
      payload_fn = function(epsilon, policy, release_context,
                            degraded = FALSE) list(
        statistic = "count",
        noisy_count = if (degraded) 0 else .dsomopDpNoisyInteger(
          true_count, policy, release_context, "distinct-person-count",
          epsilon, 1
        ),
        degraded = isTRUE(degraded)
      )
    )
  } else if (statistic == "categorical_histogram") {
    analysis <- .dsomopDpCategorical(input, spec, policy, common)
  } else if (statistic == "numeric_histogram") {
    analysis <- .dsomopDpNumericHistogram(input, spec, policy, common)
  } else if (statistic == "bounded_mean") {
    analysis <- .dsomopDpBoundedMean(input, spec, policy, common)
  } else {
    analysis <- .dsomopDpBinaryRate(input, spec, policy, common)
  }
  analysis$semantic <- c(list(
    protocol = .DSOMOP_DP_PROTOCOL,
    adjacency = policy$adjacency,
    lineage_id = input$lineage_id,
    contribution_contract = "deterministic_person_bounding_v1"
  ), analysis$semantic)
  analysis
}

#' Sticky privacy-noise service status (Aggregate)
#'
#' Returns public mechanism and accountant metadata. Secret key material and
#' protected snapshot fingerprints are never returned. Calling this endpoint
#' is an explicit service-readiness action: it coordinates bootstrap, commits
#' each missing root atomically, and transactionally creates or validates the
#' ledger.
#' A ready response includes non-secret
#' ledger, key and privacy-instance fingerprints for deployment continuity
#' checks.
#'
#' @return Public DP service status.
#' @export
omopDpStatusDS <- function() {
  .dsomopDpEnsureRuntime()
  status <- .dsomopDpPublicStatus(initialize = TRUE)
  status$supported_statistics <- .DSOMOP_DP_STATISTICS
  status$longitudinal_contract <- "deterministic_person_bounding_v1"
  status$budget_behavior <- if (!isTRUE(status$enabled)) {
    "disabled"
  } else if (isTRUE(status$bounded_accounting)) {
    "degrade_to_data_independent_zero_no_error"
  } else {
    "fixed_epsilon_no_budget_exhaustion_error_unbounded_composition"
  }
  status$person_local_provenance_required <- TRUE
  status$provenance_protocol <- .DSOMOP_DP_PROVENANCE_PROTOCOL
  status$ohdsi_querylibrary <- .dsomopDpQueryLibraryStatus()
  .pkg_state$dp_status <- status
  status
}

#' Release one sticky, person-bounded privacy statistic (Aggregate)
#'
#' @param x A server-created, pseudonymous \code{omop.table} carrying an
#'   authenticated person-local provenance capsule from an audited plan,
#'   loader, or manipulation path.
#' @param spec Typed DP statistic specification, usually JSON-encoded by
#'   dsOMOPClient.
#' @return A sticky noisy aggregate with explicit certification metadata. The
#'   current built-in sampler is reported as non-formal DP.
#' @export
omopDpReleaseDS <- function(x, spec) {
  spec <- .ds_arg(spec)
  .dsomopDpEnsureRuntime()
  policy <- .dsomopDpPolicy()
  analysis <- .dsomopDpAnalysis(x, spec, policy)
  .dsomopDpLedgerRelease(
    policy = policy, semantic = analysis$semantic,
    bounded_snapshot = analysis$snapshot,
    sensitivity = analysis$sensitivity,
    payload_fn = analysis$payload_fn
  )
}
