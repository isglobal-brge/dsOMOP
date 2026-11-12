# Module: Typed protected releases (omopDpReleaseDS, dsOMOP 2.10.3)
#
# The typed channel releases one bounded statistic of a sealed person-local
# table through the same protected-release mechanism as the automatic routes
# (tools_release.R): canonical component owners, public bases, and noise keyed
# either on a fixed public basis (unscoped pinned loader tables of a
# custodian-declared generation, for statistics whose basis does not depend on
# analyst choices) or on secret fingerprints of the realised bounded
# contributions. There is no budget, quota or history-dependent admission.
#
# Every typed statistic is a view of one registered complete public basis:
#
#   * person identity: the release-person map sealed with the table
#     (privacy_provenance.R), never a handle or key cache (B01);
#   * eligible population: a canonical identity separate from the statistic,
#     so counts, capped record counts and complete-domain partitions share one
#     person total (B02);
#   * categorical variables: the registered complete domain plus Other and
#     Unknown; requested levels are a validated view, never a mechanism input
#     (B05);
#   * numeric variables: a fixed atom basis (a registered scale's grid or the
#     universal signed nice-number grid; dates on fixed year/month atoms);
#     requested breaks snap to atom boundaries and every bin is a sum of
#     signed atoms;
#   * compound identities are injective (.dsomopFrame, B04).

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

# --- Registered complete public bases -----------------------------------------------

# Built-in complete domains of public OMOP person attributes.
.DSOMOP_TYPED_BUILTIN_DOMAINS <- list(
  gender_concept_id = c("0", "8507", "8521", "8532", "8551", "8570")
)

# Largest registered domain of a label-set (any/all) binary rate: its
# complete basis has 2^(|U| + 1) label-set patterns.
.DSOMOP_TYPED_MAX_PATTERN_DOMAIN <- 10L

.dsomopTypedValidateLabels <- function(labels, what) {
  labels <- enc2utf8(as.character(labels))
  if (!length(labels) || anyNA(labels) || any(!nzchar(labels)) ||
      any(!validUTF8(labels)) || any(nchar(labels, type = "bytes") > 256L)) {
    stop("The ", what, " must be non-empty UTF-8 labels of at most 256 ",
         "bytes.", call. = FALSE)
  }
  labels
}

#' Registered complete public domain of a typed categorical variable
#'
#' In order: the domain declared by the table's producer (sealed in its
#' provenance capsule), the custodian's \code{dsomop.release.typed_domains}
#' (named list of label vectors), and the built-in domains of public OMOP
#' person attributes. Without one the variable has no complete public basis
#' and its categorical releases are unavailable (a public refusal, decided
#' before any private value is read).
#'
#' @param capsule The table's provenance capsule (public metadata).
#' @param variable Column name.
#' @return Sorted unique character vector.
#' @keywords internal
.dsomopTypedDomain <- function(capsule, variable) {
  custom <- getOption("dsomop.release.typed_domains",
                      getOption("default.dsomop.release.typed_domains", NULL))
  if (!is.null(custom) && (!is.list(custom) || is.null(names(custom)))) {
    stop("dsomop.release.typed_domains must be a named list of label ",
         "vectors.", call. = FALSE)
  }
  declared <- if (is.list(capsule)) capsule$public_domains else NULL
  domain <- declared[[variable]] %||% custom[[variable]] %||%
    .DSOMOP_TYPED_BUILTIN_DOMAINS[[variable]]
  if (is.null(domain)) {
    stop("public_domain_unavailable: the variable '", variable, "' has no ",
         "registered complete public domain (declared by its producer, ",
         "dsomop.release.typed_domains or a built-in OMOP attribute domain), ",
         "so its categorical releases are unavailable.", call. = FALSE)
  }
  domain <- .dsomopTypedValidateLabels(domain, "registered public domain")
  sort(unique(domain), method = "radix")
}

#' Validate requested labels as a view of a registered public domain
#'
#' Duplicates and order are normalized; labels outside the domain are refused
#' independently of any private data.
#' @keywords internal
.dsomopTypedDomainView <- function(labels, domain, variable, what) {
  labels <- .dsomopTypedValidateLabels(labels, what)
  labels <- sort(unique(labels), method = "radix")
  invalid <- setdiff(labels, domain)
  if (length(invalid)) {
    stop("The requested ", what, " are not in the registered public domain ",
         "of '", variable, "' (", length(invalid), " invalid label",
         if (length(invalid) > 1L) "s" else "", "); a request may only ",
         "select labels of that domain.", call. = FALSE)
  }
  labels
}

# Universal signed "nice number" boundaries: mantissas 1, 1.5, 2, 2.5, 3, 4,
# 5, 6, 7, 8 and 9 for every decade from 1e-4 to 1e4, plus 1e5 (a fixed,
# public, data-independent basis).
.DSOMOP_TYPED_UNIVERSAL_POINTS <- local({
  m <- c(1, 1.5, 2, 2.5, 3, 4, 5, 6, 7, 8, 9)
  p <- sort(c(as.vector(outer(m, -4:4, function(m, e) {
    signif(m * 10^e, 2L)
  })), 1e5))
  c(-rev(p), 0, p)
})

.DSOMOP_TYPED_CLINICAL_DAYS <- c(0, 1, 7, 14, 21, 28, 30, 60, 90, 120, 180,
                                 270, 365, 545, 730, 1095, 1460, 1825, 3650)

#' Fixed atom basis of a typed numeric variable
#'
#' The registered scale's public grid when the variable declares one (its
#' producer, sealed in the capsule, or \code{dsomop.release.typed_scales}),
#' otherwise the universal signed nice-number grid for numbers; dates and UTC
#' datetimes use yearly atoms for 1900-1999 and monthly atoms from 2000 to the
#' end of the reference year. Atoms are \code{[p_i, p_{i+1})} plus open
#' underflow and overflow atoms; one-value releases add a missing atom.
#'
#' @keywords internal
.dsomopTypedNumericBasis <- function(capsule, variable, value_type) {
  if (value_type %in% c("date", "datetime_utc")) {
    year <- .dsomopToolsReferenceYear()
    yearly <- as.Date(sprintf("%d-01-01", 1900:1999))
    monthly <- seq(as.Date("2000-01-01"),
                   as.Date(sprintf("%d-01-01", year + 1L)), by = "month")
    days <- as.numeric(c(yearly, monthly))
    points <- if (identical(value_type, "date")) days else days * 86400
    fmt <- if (identical(value_type, "date")) {
      function(p) format(as.Date(p, origin = "1970-01-01"), "%Y-%m-%d")
    } else {
      function(p) format(as.POSIXct(p, origin = "1970-01-01", tz = "UTC"),
                         "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    }
    return(list(id = paste0("typed-", value_type, "-atoms/reference=",
                            year, "/v1"),
                points = points, format = fmt, type = value_type))
  }
  custom <- getOption("dsomop.release.typed_scales",
                      getOption("default.dsomop.release.typed_scales", NULL))
  declared <- if (is.list(capsule)) capsule$public_scales else NULL
  scale <- declared[[variable]] %||% custom[[variable]]
  fmt <- function(p) p
  if (is.null(scale)) {
    return(list(id = "typed-number-signed-nice-v1",
                points = .DSOMOP_TYPED_UNIVERSAL_POINTS, format = fmt,
                type = "number"))
  }
  scale <- as.character(scale)[[1L]]
  bounds <- .dsomopTypedScales()[[scale]]
  if (is.null(bounds)) {
    stop("public_scale_unavailable: the declared scale '", scale, "' of '",
         variable, "' is not registered.", call. = FALSE)
  }
  points <- switch(scale,
    age_years = 0:110,
    duration_days = sort(unique(c(.DSOMOP_TYPED_CLINICAL_DAYS, 7300, 10957))),
    tar_days = sort(unique(c(30 * (0:121), .DSOMOP_TYPED_CLINICAL_DAYS,
                             3650))),
    count_per_person = c(0:20, 25, 30, 40, 50, 75, 100),
    probability = round(seq(0, 1, by = 0.01), 2),
    seq(bounds[[1L]], bounds[[2L]], length.out = 101L))
  list(id = paste0("typed-number-scale=", scale, "-v1"),
       points = as.numeric(points), format = fmt, type = "number")
}

# Atom index of each value (1 = underflow, length(points) + 1 = overflow).
.dsomopTypedAtomOf <- function(v, points) findInterval(v, points) + 1L

.dsomopTypedBoundaryText <- function(p, basis) {
  if (identical(basis$type, "number")) {
    format(p, digits = 15, scientific = FALSE, trim = TRUE)
  } else basis$format(p)
}

.dsomopTypedAtomKeys <- function(basis) {
  text <- .dsomopTypedBoundaryText(basis$points, basis)
  paste0("stratum=root|atom=[", c("-inf", text), ",", c(text, "inf"), ")")
}

#' Snap requested breaks to the atom boundaries of a typed basis
#'
#' Nearest boundary, ties to the smaller one; duplicates collapse; the outer
#' breaks only bound the open first and last bins. Each view bin is a set of
#' atoms, so every coarsening is a sum of the same signed atoms.
#'
#' @keywords internal
.dsomopTypedViewBins <- function(breaks, basis) {
  p <- basis$points
  snapped <- vapply(as.numeric(breaks$numeric), function(v) {
    d <- abs(p - v)
    p[which(d == min(d))[[1L]]]
  }, numeric(1L))
  snapped <- sort(unique(snapped))
  if (length(snapped) < 2L) {
    stop("The histogram breaks collapse to fewer than two points of the ",
         "public atom basis.", call. = FALSE)
  }
  m <- length(snapped)
  cuts <- c(-Inf, snapped[-c(1L, m)], Inf)
  atom_lo <- c(-Inf, p)
  atom_hi <- c(p, Inf)
  index <- lapply(seq_len(length(cuts) - 1L), function(j) {
    which(atom_lo >= cuts[[j]] & atom_hi <= cuts[[j + 1L]])
  })
  text <- function(v) if (is.infinite(v)) {
    if (v < 0) "-inf" else "inf"
  } else .dsomopTypedBoundaryText(v, basis)
  keys <- vapply(seq_along(index), function(j) {
    paste0("stratum=root|bin=[", text(cuts[[j]]), ",", text(cuts[[j + 1L]]),
           ")")
  }, character(1L))
  public <- if (identical(basis$type, "number")) snapped else
    basis$format(snapped)
  list(index = index, public = public, keys = keys)
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

#' Canonical eligible-population identity of a typed release
#'
#' The sealed lineage plus a public eligibility rule, never the statistic,
#' bundle, variable presentation or display (B02): counts, capped record
#' counts and complete-domain partitions of one table share
#' \code{list(kind = "all_persons")} and hence one person total; a genuinely
#' different eligibility (people with a usable value) is a different
#' population.
#' @keywords internal
.dsomopTypedPopulation <- function(lineage_id, eligibility) {
  .dsomopFrame("typed-population/v1", lineage_id, eligibility)
}

# The shared person total of an eligible population.
.dsomopTypedPersonTotal <- function(people, population) {
  .dsomopTypedComponent(
    .DSOMOP_RELEASE_N_OWNER, "N", "stratum=root", length(people), people,
    rep(1, length(people)), .DSOMOP_RELEASE_FRACTIONS$N, 1, population)
}

# Person total plus a partition of persons into the cells of a complete basis.
.dsomopTypedPartition <- function(owner, population, person_tokens, cell_of,
                                  coordinates) {
  frac <- .DSOMOP_RELEASE_FRACTIONS
  comps <- list(.dsomopTypedPersonTotal(person_tokens, population))
  for (k in seq_along(coordinates)) {
    hit <- which(cell_of == k)
    comps[[length(comps) + 1L]] <- .dsomopTypedComponent(
      owner, "H", coordinates[[k]], length(hit), person_tokens[hit],
      rep(1, length(hit)), frac$H$H, 1, population)
  }
  comps
}

# Bounded weighted cells (multi-label presence or capped records): one
# person's contributions sum to at most K over the whole complete basis, at the
# full reference calibration.
.dsomopTypedWeighted <- function(owner, population, cell_maps, coordinates,
                                 K) {
  lapply(seq_along(coordinates), function(k) {
    m <- cell_maps[[k]]
    .dsomopTypedComponent(owner, "W", coordinates[[k]], sum(m$w), m$token,
                          m$w, 1, K, population)
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

# Complete categorical basis: domain labels, Other and (one-label) Unknown.
.dsomopTypedLabelCoordinates <- function(domain, unknown = TRUE) {
  c(paste0("stratum=root|cell=", domain), "stratum=root|other",
    if (unknown) "stratum=root|unknown")
}

# Cell index of each selected label on the complete basis (Other for labels
# outside the domain, NA for no label).
.dsomopTypedLabelCell <- function(label, domain) {
  i <- match(label, domain)
  i[is.na(i) & !is.na(label)] <- length(domain) + 1L
  i
}

#' Build the components and displayed values of one typed statistic
#'
#' @param input Validated input (\code{.dsomopDpValidateInput}).
#' @param common Parsed public specification (\code{.dsomopDpParseSpec}).
#' @param policy DP policy.
#' @param person_tokens Sealed release-person token of every row of
#'   \code{input$x}.
#' @param capsule Verified provenance capsule (lineage, public metadata).
#' @return List: components (the complete basis), view (indices of the
#'   displayed cells into the basis cells, or NULL), display function, owner
#'   and family.
#' @keywords internal
.dsomopTypedAnalysis <- function(input, common, policy, person_tokens,
                                 capsule) {
  statistic <- common$statistic
  K <- as.integer(policy$max_contributions)
  groups <- .dsomopDpOrderedGroups(person_tokens)
  people <- names(groups)
  lineage <- capsule$lineage_id
  all_persons <- .dsomopTypedPopulation(lineage, list(kind = "all_persons"))
  if (statistic == "count") {
    comps <- list(.dsomopTypedPersonTotal(people, all_persons))
    display <- function(noisy) list(noisy_count = max(0, noisy[[1L]]))
    return(list(components = comps, display = display, owner = "count",
                family = "N"))
  }
  if (statistic == "bounded_record_count") {
    rows <- vapply(groups, length, integer(1L))
    coordinates <- paste0("stratum=root|records=", seq_len(K))
    comps <- .dsomopTypedPartition(
      paste0("typed/record-multiplicity/K=", K, "/v1"), all_persons, people,
      pmin(rows, K), coordinates)
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
  if (statistic %in% c("categorical_histogram", "bounded_distinct",
                       "binary_rate")) {
    labels <- enc2utf8(as.character(values))
    domain <- common$domain
    reducer <- common$reducer
    canonical_labels <- function(idx) {
      sort(unique(labels[idx][!is.na(labels[idx])]), method = "radix")
    }
    if (statistic == "bounded_distinct") {
      k <- common$max_contributions
      selected <- lapply(groups, function(idx) {
        cand <- canonical_labels(idx)
        utils::head(cand[cand %in% domain], k)
      })
      has <- which(lengths(selected) > 0L)
      # One injective record per person: the framed (person, sorted labels),
      # hashed so the map key carries no label bytes (B04).
      map_tokens <- vapply(has, function(i) {
        as.character(openssl::sha256(.dsomopFrame(
          "typed-distinct-map/v1", people[[i]],
          sort(selected[[i]], method = "radix"))))
      }, character(1L))
      d <- length(unique(unlist(selected, use.names = FALSE)))
      comps <- list(.dsomopTypedComponent(
        "typed/bounded-distinct/v2", "D", "distinct", d, map_tokens,
        rep(1, length(map_tokens)), 1, K, all_persons))
      display <- function(noisy) list(
        noisy_count = min(length(domain), max(0, noisy[[1L]])),
        reducer = "distinct", max_contributions = k,
        domain_size = length(domain),
        selection_order = "canonical_utf8_value_radix",
        value_type = "categorical_utf8_v1")
      return(list(components = comps, display = display,
                  owner = "bounded_distinct", family = "D"))
    }
    if (statistic == "binary_rate" && reducer %in% c("any", "all")) {
      # Complete basis: every label-set pattern over the domain plus Other;
      # every positive mask is a view (sum of patterns), never new noise.
      width <- length(domain) + 1L
      code <- vapply(groups, function(idx) {
        s <- canonical_labels(idx)
        if (!length(s)) return(0)
        bits <- unique(.dsomopTypedLabelCell(s, domain))
        sum(2^(bits - 1L))
      }, numeric(1L))
      n_patterns <- 2^width
      bitsets <- lapply(seq_len(n_patterns) - 1L, function(v) {
        which(as.integer(intToBits(v))[seq_len(width)] == 1L)
      })
      coordinates <- paste0("stratum=root|pattern=", vapply(bitsets,
        function(b) {
          bits <- rep("0", width)
          bits[b] <- "1"
          paste(bits, collapse = "")
        }, character(1L)))
      comps <- .dsomopTypedPartition(
        paste0("typed/label-set-pattern/width=", width, "/v1"),
        all_persons, people, code + 1, coordinates)
      positive_bits <- match(common$positive, domain)
      pattern_hit <- vapply(bitsets, function(bits) {
        if (!length(bits)) return(FALSE)
        if (reducer == "any") any(bits %in% positive_bits) else
          all(bits %in% positive_bits)
      }, logical(1L))
      denominator <- common$denominator
      display <- function(noisy) {
        n <- max(0, noisy[[1L]])
        h <- .dsomopReleaseSimplex(noisy[-1L], n)
        num <- sum(h[pattern_hit])
        den <- if (denominator == "nonmissing") n - h[[1L]] else n
        list(noisy_numerator = .dsomopReleaseRound(num),
             noisy_denominator = .dsomopReleaseRound(max(0, den)),
             value = if (den > 0) .dsomopReleaseRound(num / den) else NULL,
             reducer = reducer, denominator = denominator,
             value_type = "categorical_utf8_v1")
      }
      return(list(components = comps, display = display,
                  owner = "label_set_pattern", family = "H"))
    }
    if (statistic == "categorical_histogram" &&
        reducer %in% c("presence", "records")) {
      k <- common$max_contributions
      selected <- if (reducer == "presence") {
        lapply(groups, function(idx) utils::head(canonical_labels(idx), k))
      } else {
        lapply(groups, function(idx) {
          idx <- idx[!is.na(labels[idx]) & !is.na(common$order_value[idx])]
          if (!length(idx)) return(character(0))
          idx <- idx[order(common$order_value[idx], labels[idx],
                           method = "radix")]
          labels[utils::head(idx, k)]
        })
      }
      coordinates <- .dsomopTypedLabelCoordinates(domain, unknown = FALSE)
      long_token <- rep(people, lengths(selected))
      long_cell <- .dsomopTypedLabelCell(unlist(selected, use.names = FALSE),
                                         domain)
      maps <- .dsomopTypedCellMaps(long_token, long_cell,
                                   rep(1, length(long_cell)),
                                   length(coordinates))
      owner <- paste0("typed/categorical-", reducer, "-multi/K=", K, "/v2")
      comps <- .dsomopTypedWeighted(owner, all_persons, maps, coordinates, K)
      view <- as.list(match(common$levels, domain))
      levels <- common$levels
      display <- function(noisy) list(
        levels = levels,
        counts = .dsomopReleaseRound(pmax(0, noisy[unlist(view)])),
        reducer = reducer, max_contributions = k,
        value_type = "categorical_utf8_v1")
      return(list(components = comps, display = display, owner = owner,
                  family = "W", view = view))
    }
    # One label per person (first / last / mode) on the complete basis.
    selected <- vapply(groups, function(idx) {
      if (reducer == "mode") {
        cand <- labels[idx][!is.na(labels[idx])]
        if (!length(cand)) return(NA_character_)
        f <- table(cand)
        return(sort(names(f)[f == max(f)], method = "radix")[[1L]])
      }
      i <- .dsomopDpPickIndex(idx, labels, common$order_value,
                              reducer == "last")
      if (length(i)) labels[[i]] else NA_character_
    }, character(1L))
    cell_of <- .dsomopTypedLabelCell(selected, domain)
    cell_of[is.na(cell_of)] <- length(domain) + 2L
    coordinates <- .dsomopTypedLabelCoordinates(domain, unknown = TRUE)
    owner <- paste0("typed/categorical-one-label/reducer=", reducer, "/v2")
    comps <- .dsomopTypedPartition(owner, all_persons, people, cell_of,
                                   coordinates)
    if (statistic == "binary_rate") {
      # A view of the same one-label partition as the categorical histogram.
      positive <- match(common$positive, domain)
      denominator <- common$denominator
      unknown <- length(domain) + 2L
      display <- function(noisy) {
        n <- max(0, noisy[[1L]])
        h <- .dsomopReleaseSimplex(noisy[-1L], n)
        num <- sum(h[positive])
        den <- if (denominator == "nonmissing") n - h[[unknown]] else n
        list(noisy_numerator = .dsomopReleaseRound(num),
             noisy_denominator = .dsomopReleaseRound(max(0, den)),
             value = if (den > 0) .dsomopReleaseRound(num / den) else NULL,
             reducer = reducer, denominator = denominator,
             value_type = "categorical_utf8_v1")
      }
      return(list(components = comps, display = display, owner = owner,
                  family = "H"))
    }
    view <- as.list(match(common$levels, domain))
    levels <- common$levels
    display <- function(noisy) {
      n <- max(0, noisy[[1L]])
      h <- .dsomopReleaseSimplex(noisy[-1L], n)
      list(levels = levels,
           counts = .dsomopReleaseRound(h[unlist(view)]),
           reducer = reducer, max_contributions = 1L,
           value_type = "categorical_utf8_v1")
    }
    return(list(components = comps, display = display, owner = owner,
                family = "H", view = view))
  }
  if (statistic == "numeric_histogram") {
    numeric <- .dsomopDpNumericValues(values)
    basis <- .dsomopTypedNumericBasis(capsule, common$variable, numeric$type)
    bins <- .dsomopTypedViewBins(common$breaks, basis)
    atoms <- .dsomopTypedAtomKeys(basis)
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
      maps <- .dsomopTypedCellMaps(
        rep(people, lengths(long)),
        .dsomopTypedAtomOf(unlist(long, use.names = FALSE), basis$points),
        rep(1, sum(lengths(long))), length(atoms))
      owner <- paste0("typed/numeric-records/basis=", basis$id, "/K=", K,
                      "/v2")
      comps <- .dsomopTypedWeighted(owner, all_persons, maps, atoms, K)
      display <- function(noisy) list(
        breaks = bins$public,
        counts = .dsomopReleaseRound(vapply(bins$index, function(i) {
          max(0, sum(noisy[i]))
        }, numeric(1L))),
        reducer = reducer, max_contributions = k,
        value_type = numeric$type,
        interval_contract = "left_closed_right_open_first_last_open")
      return(list(components = comps, display = display, owner = owner,
                  family = "W", view = bins$index, view_keys = bins$keys,
                  effective_breaks = bins$public, basis_id = basis$id))
    }
    reduced <- .dsomopDpReduceOne(person_tokens, numeric$value, reducer,
                                  common$order_value)
    cell_of <- rep(length(atoms) + 1L, length(people))
    cell_of[match(reduced$person, people)] <- .dsomopTypedAtomOf(
      as.numeric(reduced$value), basis$points)
    owner <- paste0("typed/numeric-one-value/reducer=", reducer, "/basis=",
                    basis$id, "/v2")
    comps <- .dsomopTypedPartition(owner, all_persons, people, cell_of,
                                   c(atoms, "stratum=root|missing"))
    display <- function(noisy) {
      n <- max(0, noisy[[1L]])
      h <- .dsomopReleaseSimplex(noisy[-1L], n)
      list(breaks = bins$public,
           counts = .dsomopReleaseRound(vapply(bins$index, function(i) {
             sum(h[i])
           }, numeric(1L))),
           reducer = reducer, max_contributions = 1L,
           value_type = numeric$type,
           interval_contract = "left_closed_right_open_first_last_open")
    }
    return(list(components = comps, display = display, owner = owner,
                family = "H", view = bins$index, view_keys = bins$keys,
                effective_breaks = bins$public, basis_id = basis$id))
  }
  # bounded_mean
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
  # A genuinely different eligible population (people with a usable value):
  # it shares a count's person total only through public eligibility rules.
  p <- .dsomopTypedPopulation(lineage, list(
    kind = "nonmissing", variable = common$variable,
    reducer = common$reducer, order_by = common$order_by))
  comps <- list(
    .dsomopTypedPersonTotal(reduced$person, p),
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
  list(components = comps, display = display, owner = owner, family = "M")
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
  # The release-person map sealed with the table (B01): no handle, key cache
  # or earlier operation can choose another person namespace.
  person_tokens <- .dsomopDpSealedReleaseTokens(x, input$token)
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
                                   capsule)
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
  coordinates <- vapply(analysis$components, function(c) c$coordinate,
                        character(1L))
  # Signed components of the complete basis: partition cells are linearly
  # reconciled to the one noisy person total over the whole basis (spec 4.5),
  # so the shift never depends on the requested view; exported and displayed
  # view cells are sums of these signed basis cells.
  basis <- as.numeric(noisy)
  if (identical(analysis$family, "H") && length(basis) > 1L) {
    cells <- basis[-1L]
    basis[-1L] <- cells + (basis[[1L]] - sum(cells)) / length(cells)
  }
  has_n <- identical(families[[1L]], "N") && length(basis) > 1L
  offset <- if (has_n) 1L else 0L
  view <- analysis$view
  if (is.null(view)) {
    view_signed <- basis
    view_coordinates <- coordinates
    view_families <- families
    view_index <- NULL
  } else {
    sums <- vapply(view, function(i) sum(basis[offset + i]), numeric(1L))
    keys <- analysis$view_keys %||%
      coordinates[offset + vapply(view, `[[`, integer(1L), 1L)]
    view_signed <- c(if (has_n) basis[[1L]], sums)
    view_coordinates <- c(if (has_n) coordinates[[1L]], keys)
    view_families <- c(if (has_n) families[[1L]],
                       rep(families[[offset + 1L]], length(view)))
    view_index <- lapply(view, as.integer)
  }
  payload <- c(list(
    protocol = .DSOMOP_RELEASE_PROTOCOL,
    release_contract = .DSOMOP_RELEASE_CONTRACT_VERSION,
    statistic = common$statistic,
    protection = protection,
    guarantee_scope = unname(.DSOMOP_RELEASE_GUARANTEE[protection]),
    noise_reference_epsilon = e,
    mechanism_epsilon = if (fixed) vector_epsilon else NA_real_,
    api_transcript_epsilon = NA_real_
  ),
  # A pure-DP delta is claimed only for the declared fixed vector (B14).
  if (fixed) list(delta = 0),
  list(
    adjacency = policy$adjacency,
    mechanism = policy$mechanism,
    sampler = policy$sampler,
    sticky = TRUE,
    allocation = fam_alloc,
    sensitivity = fam_sens,
    noise_sd = fam_sd,
    privacy_call_quota = "none",
    history_based_admission = FALSE,
    source = list(logical_source_id = .dsomopReleaseLogicalSourceId(policy))
  ), shown, list(
    components = list(
      owner = analysis$owner, family = analysis$family,
      coordinates = view_coordinates,
      families = view_families,
      reconciliation = if (identical(analysis$family, "H")) {
        "complete_basis_cells_sum_to_person_total"
      } else "none",
      signed = view_signed,
      basis = list(coordinates = coordinates, families = families,
                   signed = basis),
      view_of_basis = view_index)
  ))
  if (!is.null(analysis$effective_breaks)) {
    payload$requested_breaks <- common$breaks$public
    payload$basis_id <- analysis$basis_id
  }
  payload
}
