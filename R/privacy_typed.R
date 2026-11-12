# Module: Typed protected releases (omopDpReleaseDS, dsOMOP 2.10.3)
#
# The typed channel releases one bounded statistic of a sealed person-local
# table through the same protected-release mechanism as the automatic routes
# (tools_release.R): canonical component owners, public grids, and noise keyed
# either on a fixed public basis (unscoped pinned loader tables of a
# custodian-declared generation, for statistics whose basis does not depend on
# analyst choices) or on secret fingerprints of the realised bounded
# contributions. Dummy or unused levels, clamp-only outer breaks, harmless cap
# variants, reordered or duplicated public lists, renamed columns and lineage
# cosmetics therefore reuse the same components instead of drawing fresh
# noise. There is no budget, quota or history-dependent admission.

.DSOMOP_TYPED_SCALES <- list(
  age_years = c(0, 110),
  duration_days = c(0, 10957),
  tar_days = c(0, 3650),
  count_per_person = c(0, 100),
  probability = c(0, 1)
)

#' Registered public moment scales of typed bounded means
#'
#' The built-in scales plus the custodian's \code{dsomop.release.public_scales}
#' (a named list of \code{c(lower, upper)}). A typed bounded mean must request
#' one of them exactly: caller-chosen clipping bounds would mint a fresh
#' estimate per bound pair.
#'
#' @return Named list of numeric c(lower, upper).
#' @keywords internal
.dsomopTypedScales <- function() {
  scales <- .DSOMOP_TYPED_SCALES
  custom <- getOption("dsomop.release.public_scales",
                      getOption("default.dsomop.release.public_scales", NULL))
  if (!is.null(custom)) {
    if (!is.list(custom) || is.null(names(custom)) || anyNA(names(custom)) ||
        any(!grepl("^[a-z][a-z0-9_]{0,63}\\z", names(custom),
                   perl = TRUE))) {
      stop("dsomop.release.public_scales must be a named list of bounds.",
           call. = FALSE)
    }
    for (name in names(custom)) {
      b <- suppressWarnings(as.numeric(custom[[name]]))
      if (length(b) != 2L || !all(is.finite(b)) || b[[1L]] >= b[[2L]]) {
        stop("dsomop.release.public_scales entries must be c(lower, upper).",
             call. = FALSE)
      }
      scales[[name]] <- b
    }
  }
  scales
}

# --- Public grids of typed histogram breaks ---------------------------------------

# Nearest point of the 2-significant-digit decimal lattice (ties to the
# smaller value, i.e. towards minus infinity).
.dsomopTypedSnapNumber <- function(x) {
  vapply(as.numeric(x), function(v) {
    if (!is.finite(v)) stop("Histogram breaks must be finite.", call. = FALSE)
    if (abs(v) < 1e-9) return(0)
    e <- floor(log10(abs(v)))
    step <- 10^(e - 1)
    lo <- floor(v / step) * step
    hi <- lo + step
    if (abs(v - lo) <= abs(hi - v)) {
      signif(lo, 2)
    } else signif(hi, 2)
  }, numeric(1L))
}

# Nearest first day of a calendar month (ties to the earlier one).
.dsomopTypedSnapDate <- function(x) {
  d <- as.Date(x)
  first <- as.Date(format(d, "%Y-%m-01"))
  nxt <- as.Date(format(first + 32, "%Y-%m-01"))
  ifelse(as.numeric(d - first) <= as.numeric(nxt - d), first, nxt)
}

# Nearest UTC midnight (ties to the earlier one).
.dsomopTypedSnapDatetime <- function(x) {
  t <- as.numeric(as.POSIXct(x, tz = "UTC"))
  day <- floor(t / 86400) * 86400
  ifelse(t - day <= day + 86400 - t, day, day + 86400)
}

#' Effective public breaks of a typed histogram
#'
#' Requested breaks snap to the public grid of their value type (numbers: the
#' 2-significant-digit decimal lattice; dates: first days of months; UTC
#' datetimes: midnights), duplicates collapse, and the first and last bins are
#' open (values beyond the outer breaks always fell into them, so the outer
#' break values are presentation, not part of the estimand).
#'
#' @param breaks Parsed breaks (\code{.dsomopDpBreaks}).
#' @param value_type "number", "date" or "datetime_utc".
#' @return List: numeric breaks, public breaks, bin coordinate keys.
#' @keywords internal
.dsomopTypedEffectiveBreaks <- function(breaks, value_type) {
  numeric <- breaks$numeric
  snapped <- switch(value_type,
    number = .dsomopTypedSnapNumber(numeric),
    date = as.numeric(.dsomopTypedSnapDate(as.Date(numeric,
                                                   origin = "1970-01-01"))),
    datetime_utc = .dsomopTypedSnapDatetime(as.POSIXct(numeric,
                                                       origin = "1970-01-01",
                                                       tz = "UTC")))
  snapped <- sort(unique(as.numeric(snapped)))
  if (length(snapped) < 2L) {
    stop("The histogram breaks collapse to fewer than two points on the ",
         "public release grid.", call. = FALSE)
  }
  public <- switch(value_type,
    number = snapped,
    date = format(as.Date(snapped, origin = "1970-01-01"), "%Y-%m-%d"),
    datetime_utc = format(as.POSIXct(snapped, origin = "1970-01-01",
                                     tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ",
                          tz = "UTC"))
  text <- if (identical(value_type, "number")) {
    format(snapped, digits = 15, scientific = FALSE, trim = TRUE)
  } else public
  m <- length(snapped)
  lower <- c("-inf", text[2:(m - 1L)])
  upper <- c(text[2:(m - 1L)], "inf")
  if (m == 2L) { lower <- "-inf"; upper <- "inf" }
  keys <- paste0(value_type, ":[", lower, ",", upper, ")")
  list(numeric = snapped, public = public, keys = keys)
}

# --- Person identifiers of a sealed table ----------------------------------------

#' Stable person identifiers of a sealed table
#'
#' The table's person tokens are the reversible pseudonyms of its handle. When
#' the session handle carries the same pseudonymization contract they are
#' reversed to the raw CDM identifiers, so typed and automatic releases share
#' contribution maps; otherwise the pseudonym itself (namespaced) identifies the
#' person, which is stable for that resource.
#'
#' @keywords internal
.dsomopTypedPersonIds <- function(x, tokens) {
  handle <- .dsomopDpState()$dp_handle %||% .dsomopDpSessionHandle()
  table_contract <- tryCatch(
    .omopTablePseudonymization(x, caller = "omopDpReleaseDS"),
    error = function(e) NULL)
  # Only a handle whose key is already resolved (its public key id cached) is
  # compared: the lookup never creates or loads a new pseudonymization root.
  cached <- if (is.environment(handle)) handle$person_key_id else NULL
  if (is.list(table_contract) && is.character(cached) &&
      length(cached) == 1L && identical(table_contract$key_id, cached)) {
    same <- tryCatch(identical(
      table_contract,
      .canonicalPseudonymizationContract(.personKeyPublicContract(handle))
    ), error = function(e) FALSE)
    if (isTRUE(same)) {
      ids <- tryCatch(.unhashPersonKey(tokens, .personKey(handle)),
                      error = function(e) NULL)
      if (!is.null(ids) && !anyNA(ids)) {
        ids <- as.character(ids)
        sci <- grepl("e", ids, fixed = TRUE) &
          !is.na(suppressWarnings(as.numeric(ids)))
        ids[sci] <- sprintf("%.0f", as.numeric(ids[sci]))
        return(ids)
      }
    }
  }
  paste0("pseudonym:", tokens)
}

# --- Components ---------------------------------------------------------------------

# One component of a typed release.
.dsomopTypedComponent <- function(owner, family, coordinate, value, tokens,
                                  contributions, allocation, sensitivity,
                                  population) {
  list(owner = owner, family = family, coordinate = coordinate,
       value = as.numeric(value), tokens = tokens,
       contributions = as.numeric(contributions),
       allocation = allocation, sensitivity = sensitivity,
       population = population)
}

.dsomopTypedDraw <- function(policy, protection, components) {
  keys <- .dsomopReleaseKeys(policy)
  vapply(components, function(c) {
    fp <- if (identical(protection, "realised_sticky")) {
      .dsomopReleaseFingerprint(keys, c$owner, c$family, c$coordinate,
                                c$tokens, c$contributions)
    } else NULL
    .dsomopReleaseNoisyComponent(policy, keys, protection, list(
      owner = c$owner, population = c$population, family = c$family,
      coordinate = c$coordinate, value = c$value, allocation = c$allocation,
      sensitivity = c$sensitivity, fingerprint = fp))
  }, numeric(1L))
}

# Person total plus a partition of persons into public cells.
.dsomopTypedPartition <- function(owner, population, person_tokens, cell_of,
                                  cells) {
  frac <- .DSOMOP_RELEASE_FRACTIONS
  comps <- list(.dsomopTypedComponent(
    .DSOMOP_RELEASE_N_OWNER, "N", "stratum=root", length(person_tokens),
    person_tokens, rep(1, length(person_tokens)), frac$N, 1, population))
  for (k in seq_along(cells)) {
    hit <- which(cell_of == k)
    comps[[length(comps) + 1L]] <- .dsomopTypedComponent(
      owner, "H", paste0("stratum=root|cell=", cells[[k]]), length(hit),
      person_tokens[hit], rep(1, length(hit)), frac$H$H, 1, population)
  }
  comps
}

# Bounded weighted cells (multi-label presence or capped records): one
# person's contributions sum to at most K over all cells, at the full
# reference calibration.
.dsomopTypedWeighted <- function(owner, population, cell_tokens, cells, K) {
  lapply(seq_along(cells), function(k) {
    m <- cell_tokens[[k]]
    .dsomopTypedComponent(owner, "W", paste0("cell=", cells[[k]]),
                          sum(m$w), m$token, m$w, 1, K, population)
  })
}

# Per-cell (token, weight) maps from long (token, cell index, weight) rows.
.dsomopTypedCellMaps <- function(token, cell, w, ncells) {
  lapply(seq_len(ncells), function(k) {
    hit <- which(cell == k)
    if (!length(hit)) return(list(token = character(0), w = numeric(0)))
    agg <- tapply(w[hit], token[hit], sum)
    list(token = names(agg), w = as.numeric(agg))
  })
}

# The registered public scale of a typed bounded mean (a public check, made
# before any private input is inspected).
.dsomopTypedMeanScale <- function(common) {
  scales <- .dsomopTypedScales()
  match_scale <- names(scales)[vapply(scales, function(s) {
    isTRUE(all.equal(s, c(common$lower, common$upper), tolerance = 0))
  }, logical(1L))]
  if (length(match_scale) == 0L) {
    stop("public_scale_unavailable: a bounded mean needs one of the ",
         "registered public scales (", paste(vapply(names(scales),
           function(n) paste0(n, " [", scales[[n]][[1L]], ", ",
                              scales[[n]][[2L]], "]"), character(1L)),
           collapse = "; "), "); caller-chosen bounds are not accepted.",
         call. = FALSE)
  }
  match_scale[[1L]]
}

#' Build the components and displayed values of one typed statistic
#'
#' @param input Validated input (\code{.dsomopDpValidateInput}).
#' @param common Parsed public specification (\code{.dsomopDpParseSpec}).
#' @param policy DP policy.
#' @param person_tokens Release person token of every row of \code{input$x}.
#' @param population Fixed-basis population id (lineage plus eligibility).
#' @return List: components, display function, payload fields, owner label.
#' @keywords internal
.dsomopTypedAnalysis <- function(input, common, policy, person_tokens,
                                 population_base) {
  statistic <- common$statistic
  K <- as.integer(policy$max_contributions)
  groups <- .dsomopDpOrderedGroups(person_tokens)
  people <- names(groups)
  pop <- function(...) paste(c(population_base, statistic, ...),
                             collapse = "|")
  if (statistic == "count") {
    comps <- list(.dsomopTypedComponent(
      .DSOMOP_RELEASE_N_OWNER, "N", "stratum=root", length(people), people,
      rep(1, length(people)), .DSOMOP_RELEASE_FRACTIONS$N, 1, pop()))
    display <- function(noisy) list(noisy_count = max(0, noisy[[1L]]))
    return(list(components = comps, display = display, owner = "count",
                family = "N"))
  }
  if (statistic == "bounded_record_count") {
    rows <- vapply(groups, length, integer(1L))
    cells <- paste0("records=", seq_len(K))
    cell_of <- pmin(rows, K)
    comps <- .dsomopTypedPartition(
      paste0("typed/record-multiplicity/K=", K, "/v1"), pop(), people,
      cell_of, cells)
    k <- common$max_contributions
    display <- function(noisy) {
      n <- max(0, noisy[[1L]])
      h <- .dsomopReleaseSimplex(noisy[-1L], n)
      list(noisy_count = .dsomopReleaseRound(sum(pmin(seq_len(K), k) * h)),
           reducer = "records", max_contributions = k)
    }
    return(list(components = comps, display = display,
                owner = "record_multiplicity", family = "H"))
  }
  values <- if (!is.null(common$variable)) input$x[[common$variable]] else NULL
  if (statistic %in% c("categorical_histogram", "bounded_distinct")) {
    labels <- enc2utf8(as.character(values))
    levels <- common$levels
    reducer <- common$reducer
    k <- common$max_contributions
    if (statistic == "bounded_distinct" || reducer %in% c("presence")) {
      selected <- lapply(groups, function(idx) {
        cand <- sort(unique(labels[idx][!is.na(labels[idx]) &
                                          labels[idx] %in% levels]),
                     method = "radix")
        utils::head(cand, k)
      })
    } else if (reducer == "records") {
      selected <- lapply(groups, function(idx) {
        idx <- idx[!is.na(labels[idx]) & labels[idx] %in% levels &
                     !is.na(common$order_value[idx])]
        if (!length(idx)) return(character(0))
        idx <- idx[order(common$order_value[idx], labels[idx],
                         method = "radix")]
        labels[utils::head(idx, k)]
      })
    } else if (reducer == "mode") {
      selected <- lapply(groups, function(idx) {
        cand <- labels[idx][!is.na(labels[idx]) & labels[idx] %in% levels]
        if (!length(cand)) return(character(0))
        f <- table(cand)
        sort(names(f)[f == max(f)], method = "radix")[[1L]]
      })
    } else {
      selected <- lapply(groups, function(idx) {
        i <- .dsomopDpPickIndex(idx, labels, common$order_value,
                                reducer == "last")
        if (length(i) && labels[[i]] %in% levels) labels[[i]] else
          character(0)
      })
    }
    if (statistic == "bounded_distinct") {
      has <- lengths(selected) > 0L
      map_tokens <- paste0(people[has], "|", vapply(selected[has], function(s) {
        paste(sort(s, method = "radix"), collapse = "\u001f")
      }, character(1L)))
      d <- length(unique(unlist(selected, use.names = FALSE)))
      comps <- list(.dsomopTypedComponent(
        "typed/bounded-distinct/v1", "D", "distinct", d, map_tokens,
        rep(1, length(map_tokens)), 1, K, pop(common$variable)))
      display <- function(noisy) list(
        noisy_count = min(length(levels), max(0, noisy[[1L]])),
        reducer = "distinct", max_contributions = k,
        domain_size = length(levels),
        selection_order = "canonical_utf8_value_radix",
        value_type = "categorical_utf8_v1")
      return(list(components = comps, display = display,
                  owner = "bounded_distinct", family = "D"))
    }
    one_label <- reducer %in% c("mode", "first", "last") ||
      (reducer == "presence" && k == 1L)
    if (one_label) {
      has <- lengths(selected) > 0L
      cell_of <- match(vapply(selected[has], `[[`, character(1L), 1L), levels)
      owner <- paste0("typed/categorical-one-label/reducer=",
                      if (reducer == "presence") "first_canonical" else reducer,
                      "/v1")
      comps <- .dsomopTypedPartition(owner, pop(common$variable, reducer,
                                                paste(levels, collapse = "\u001f")),
                                     people[has], cell_of, levels)
      display <- function(noisy) {
        n <- max(0, noisy[[1L]])
        list(levels = levels,
             counts = .dsomopReleaseRound(.dsomopReleaseSimplex(noisy[-1L], n)),
             reducer = reducer, max_contributions = k,
             value_type = "categorical_utf8_v1")
      }
      return(list(components = comps, display = display, owner = owner,
                  family = "H"))
    }
    long_token <- rep(people, lengths(selected))
    long_cell <- match(unlist(selected, use.names = FALSE), levels)
    maps <- .dsomopTypedCellMaps(long_token, long_cell,
                                 rep(1, length(long_cell)), length(levels))
    owner <- paste0("typed/categorical-", reducer, "-multi/K=", K, "/v1")
    comps <- .dsomopTypedWeighted(owner, pop(common$variable, reducer), maps,
                                  levels, K)
    display <- function(noisy) list(
      levels = levels, counts = .dsomopReleaseRound(pmax(0, noisy)),
      reducer = reducer, max_contributions = k,
      value_type = "categorical_utf8_v1")
    return(list(components = comps, display = display, owner = owner,
                family = "W"))
  }
  if (statistic == "numeric_histogram") {
    numeric <- .dsomopDpNumericValues(values)
    effective <- .dsomopTypedEffectiveBreaks(common$breaks, numeric$type)
    b <- effective$numeric
    bin_of <- function(v) {
      i <- findInterval(v, b)
      i[i < 1L] <- 1L
      pmin(i, length(b) - 1L)
    }
    reducer <- common$reducer
    if (reducer == "records") {
      k <- common$max_contributions
      long <- lapply(groups, function(idx) {
        idx <- idx[!is.na(numeric$value[idx])]
        if (!is.null(common$order_value)) {
          idx <- idx[!is.na(common$order_value[idx])]
          idx <- idx[order(common$order_value[idx], numeric$value[idx],
                           method = "radix")]
        } else {
          idx <- idx[order(numeric$value[idx], method = "radix")]
        }
        numeric$value[utils::head(idx, k)]
      })
      maps <- .dsomopTypedCellMaps(rep(people, lengths(long)),
                                   bin_of(unlist(long, use.names = FALSE)),
                                   rep(1, sum(lengths(long))),
                                   length(effective$keys))
      owner <- paste0("typed/numeric-records/type=", numeric$type, "/K=", K,
                      "/v1")
      comps <- .dsomopTypedWeighted(owner, pop(common$variable), maps,
                                    effective$keys, K)
      display <- function(noisy) list(
        breaks = effective$public, counts = .dsomopReleaseRound(pmax(0, noisy)),
        reducer = reducer, max_contributions = k,
        value_type = numeric$type,
        interval_contract = "left_closed_right_open_first_last_open")
      return(list(components = comps, display = display, owner = owner,
                  family = "W", effective_breaks = effective$public))
    }
    reduced <- .dsomopDpReduceOne(person_tokens, numeric$value, reducer,
                                  common$order_value)
    owner <- paste0("typed/numeric-one-value/reducer=", reducer, "/type=",
                    numeric$type, "/v1")
    comps <- .dsomopTypedPartition(owner, pop(common$variable, reducer),
                                   reduced$person,
                                   bin_of(as.numeric(reduced$value)),
                                   effective$keys)
    display <- function(noisy) {
      n <- max(0, noisy[[1L]])
      list(breaks = effective$public,
           counts = .dsomopReleaseRound(.dsomopReleaseSimplex(noisy[-1L], n)),
           reducer = reducer, max_contributions = 1L,
           value_type = numeric$type,
           interval_contract = "left_closed_right_open_first_last_open")
    }
    return(list(components = comps, display = display, owner = owner,
                family = "H", effective_breaks = effective$public))
  }
  if (statistic == "bounded_mean") {
    scale <- .dsomopTypedMeanScale(common)
    numeric <- .dsomopDpNumericValues(values)
    reduced <- .dsomopDpReduceOne(person_tokens, numeric$value, common$reducer,
                                  common$order_value)
    lattice <- .dsomopReleaseLattice(as.numeric(reduced$value),
                                     c(common$lower, common$upper))
    frac <- .DSOMOP_RELEASE_FRACTIONS
    J <- .DSOMOP_RELEASE_LATTICE
    owner <- paste0("typed/bounded-moments/scale=", scale, "/reducer=",
                    common$reducer, "/v1")
    p <- pop(common$variable, common$reducer, scale)
    comps <- list(
      .dsomopTypedComponent(.DSOMOP_RELEASE_N_OWNER, "N", "stratum=root",
                            length(reduced$person), reduced$person,
                            rep(1, length(reduced$person)), frac$N, 1, p),
      .dsomopTypedComponent(owner, "S", "stratum=root", sum(lattice$s),
                            reduced$person, lattice$s, frac$M$S, J, p),
      .dsomopTypedComponent(owner, "Q", "stratum=root", sum(lattice$q),
                            reduced$person, lattice$q, frac$M$Q, J, p))
    L <- common$lower; U <- common$upper
    display <- function(noisy) {
      n <- max(0, noisy[[1L]])
      mean <- sd <- NA_real_
      if (n >= 1) {
        pr <- .dsomopReleaseProjectMoments(noisy[[2L]] / J, noisy[[3L]] / J, n)
        mean <- (L + U) / 2 + (U - L) / 2 * pr[["S"]] / n
        if (n >= 2) {
          sd <- sqrt(max(0, (pr[["Q"]] - pr[["S"]]^2 / n) / (n - 1))) *
            (U - L) / 2
        }
      }
      list(noisy_count = n, value = .dsomopReleaseRound(mean),
           sd = .dsomopReleaseRound(sd), lower = L, upper = U, scale = scale,
           reducer = common$reducer, value_type = "number")
    }
    return(list(components = comps, display = display, owner = owner,
                family = "M"))
  }
  # binary_rate: a {positive, negative} partition of the eligible persons.
  labels <- enc2utf8(as.character(values))
  positive <- common$positive
  reducer <- common$reducer
  eligible <- logical(length(groups))
  hit <- logical(length(groups))
  for (i in seq_along(groups)) {
    idx <- groups[[i]]
    observed <- labels[idx]
    nonmissing <- !is.na(observed)
    if (common$denominator == "nonmissing" && !any(nonmissing)) next
    eligible[[i]] <- TRUE
    observed <- observed[nonmissing]
    hit[[i]] <- if (length(observed) == 0L) FALSE else if (reducer == "any") {
      any(observed %in% positive)
    } else if (reducer == "all") {
      all(observed %in% positive)
    } else {
      j <- .dsomopDpPickIndex(idx, labels, common$order_value,
                              reducer == "last")
      length(j) == 1L && labels[[j]] %in% positive
    }
  }
  owner <- paste0("typed/binary/reducer=", reducer, "/denominator=",
                  common$denominator, "/v1")
  comps <- .dsomopTypedPartition(owner, pop(common$variable, reducer,
                                            common$denominator,
                                            paste(positive, collapse = "\u001f")),
                                 people[eligible],
                                 ifelse(hit[eligible], 1L, 2L),
                                 c("positive", "negative"))
  display <- function(noisy) {
    n <- max(0, noisy[[1L]])
    h <- .dsomopReleaseSimplex(noisy[-1L], n)
    list(noisy_numerator = .dsomopReleaseRound(h[[1L]]),
         noisy_denominator = n,
         value = if (n > 0) .dsomopReleaseRound(h[[1L]] / n) else NULL,
         reducer = reducer, denominator = common$denominator,
         value_type = "categorical_utf8_v1")
  }
  list(components = comps, display = display, owner = owner, family = "H")
}

#' Run one typed protected release
#'
#' @param x Sealed omop.table.
#' @param spec Typed specification.
#' @param policy DP policy.
#' @return Public payload.
#' @keywords internal
.dsomopTypedRelease <- function(x, spec, policy) {
  common <- .dsomopDpParseSpec(x, spec, policy)
  if (identical(common$statistic, "bounded_mean")) .dsomopTypedMeanScale(common)
  input <- .dsomopDpValidateInput(x, policy)
  common$order_value <- .dsomopDpOrderVector(input$x, common$order_by)
  capsule <- attr(x, "dsomop_dp_provenance", exact = TRUE)
  ids <- .dsomopTypedPersonIds(x, input$token)
  keys <- .dsomopReleaseKeys(policy)
  person_tokens <- if (length(ids)) .dsomopReleasePersonTokens(
    keys, policy, ids) else character(0)
  # Fixed basis only for statistics whose basis does not depend on analyst
  # choices, over an unscoped pinned loader table of a custodial generation.
  fixed_capable <- common$statistic %in% c("count", "bounded_record_count",
                                           "bounded_mean") &&
    startsWith(capsule$producer %||% "", "query/") &&
    isTRUE(policy$custodial_snapshot)
  mode <- .dsomopToolsMode()
  if (identical(mode, "dp") && !fixed_capable) {
    stop("This typed release has no fixed-synopsis protection: DP-only mode ",
         "(dsomop.tools.mode = \"dp\" or dsomop.dp.exclusive = TRUE) accepts ",
         "typed counts, capped record counts and registered-scale bounded ",
         "means of unscoped pinned loader tables of a custodian-declared data ",
         "generation. In auto mode the same release uses heuristic ",
         "realised-contribution protection.", call. = FALSE)
  }
  protection <- if (fixed_capable) "fixed_synopsis" else "realised_sticky"
  analysis <- .dsomopTypedAnalysis(input, common, policy, person_tokens,
                                   population_base = capsule$lineage_id)
  noisy <- .dsomopTypedDraw(policy, protection, analysis$components)
  shown <- analysis$display(noisy)
  e <- policy$release_epsilon
  families <- vapply(analysis$components, function(c) c$family, character(1L))
  first_of <- !duplicated(families)
  fam_alloc <- stats::setNames(lapply(analysis$components[first_of],
                                      function(c) c$allocation * e),
                               families[first_of])
  fam_sens <- stats::setNames(lapply(analysis$components[first_of],
                                     function(c) c$sensitivity),
                              families[first_of])
  fam_sd <- stats::setNames(lapply(analysis$components[first_of], function(c) {
    s <- .dsomopReleaseNoiseSd(c$allocation * e, c$sensitivity)
    if (c$family %in% c("S", "Q")) s / .DSOMOP_RELEASE_LATTICE else s
  }), families[first_of])
  fixed <- identical(protection, "fixed_synopsis")
  # The declared bounded vector's epsilon: one allocation per family.
  vector_epsilon <- sum(unlist(fam_alloc))
  # Signed components for pooling: partition cells are linearly reconciled to
  # their one noisy person total (spec 4.5), so no second, independent total
  # is exported. Each label's or bin's underlying component is canonical: it
  # is drawn from its own coordinate and contributions, whatever other levels
  # are declared, so declared levels only change this shift (post-processing
  # of fixed components), never redraw a cell.
  signed <- as.numeric(noisy)
  if (identical(analysis$family, "H") && length(signed) > 1L) {
    cells <- signed[-1L]
    signed[-1L] <- cells + (signed[[1L]] - sum(cells)) / length(cells)
  }
  payload <- c(list(
    protocol = .DSOMOP_RELEASE_PROTOCOL,
    release_contract = .DSOMOP_RELEASE_CONTRACT_VERSION,
    statistic = common$statistic,
    protection = protection,
    guarantee_scope = unname(.DSOMOP_RELEASE_GUARANTEE[protection]),
    noise_reference_epsilon = e,
    mechanism_epsilon = if (fixed) vector_epsilon else NA_real_,
    api_transcript_epsilon = NA_real_,
    delta = 0,
    adjacency = policy$adjacency,
    mechanism = policy$mechanism,
    sampler = policy$sampler,
    sticky = TRUE,
    allocation = fam_alloc,
    sensitivity = fam_sens,
    noise_sd = fam_sd,
    privacy_call_quota = "none",
    history_based_admission = FALSE
  ), shown, list(
    components = list(
      owner = analysis$owner, family = analysis$family,
      coordinates = vapply(analysis$components, function(c) c$coordinate,
                           character(1L)),
      families = families,
      reconciliation = if (identical(analysis$family, "H")) {
        "declared_cells_sum_to_person_total"
      } else "none",
      signed = signed)
  ))
  if (!is.null(analysis$effective_breaks)) {
    payload$requested_breaks <- common$breaks$public
  }
  payload
}
