# Module: Protected releases of the OHDSI community tools (dsOMOP 2.10.3)
#
# The OHDSI community tools release, for a closed set of reviewed estimands
# (distributions of one bounded value per person, sparse demographic
# partitions, person-time and time-to-event summaries), protected components
# instead of exact summaries. There is no privacy accountant, total-epsilon
# budget, release quota, rate limit or per-user/session admission state: an
# answer depends only on the current request, the data and the server
# configuration, never on how many questions were asked before.
#
# Averaging attacks are closed by determinism, not by refusing questions:
#
#   * canonical estimand identity: aliases, renamed or reordered variants and
#     view-only parameters reuse exactly the same protected components;
#   * public quantization: free numeric parameters snap to fixed public grids,
#     so nearby requests share one effective estimand and one noise draw;
#   * fixed public basis ("fixed_synopsis"): an unscoped question over a
#     public atom basis of an immutable, custodian-declared data generation
#     draws its noise from (corpus, generation, owner, atom) only, so every
#     view is post-processing of one protected vector;
#   * realised contributions ("realised_sticky"): any other supported scope
#     keys each component's noise on a secret fingerprint of the bounded
#     contributions of the people it contains, so different formulations of
#     the same component replay its noise. This second route is a heuristic
#     disclosure control, not a joint differential-privacy guarantee.
#
# Components use the exact discrete-Laplace sampler (privacy_sampler.R) with
# fixed allocations of the reference calibration e (dsomop.dp.release_epsilon,
# 0.1 by default): person totals 0.2e wherever they occur; histograms 0.8e;
# histogram+moments 0.4e/0.2e/0.2e; moments 0.4e/0.4e; person-time status
# 0.3e and time 0.5e. Allocations are static calibration, not accounting.

.DSOMOP_RELEASE_PROTOCOL <- "dsomop-protected-release-v1"
.DSOMOP_RELEASE_CONTRACT_VERSION <- "dsomop-release-contract-v1"
.DSOMOP_RELEASE_LATTICE <- 65535
.DSOMOP_RELEASE_TIME_CAP <- 3650
.DSOMOP_RELEASE_MULTI_CAP <- 10L
.DSOMOP_RELEASE_N_OWNER <- "eligible-person-count/v1"
.DSOMOP_RELEASE_FRACTIONS <- list(
  N = 0.2,
  H = list(H = 0.8),
  HM = list(H = 0.4, S = 0.2, Q = 0.2),
  M = list(S = 0.4, Q = 0.4),
  C6 = list(status = 0.3, time = 0.5)
)
.DSOMOP_RELEASE_PROTECTED <- c("fixed_synopsis", "realised_sticky")
.DSOMOP_RELEASE_GUARANTEE <- c(
  fixed_synopsis = "fixed_vector_before_exact_support_gate",
  realised_sticky = "heuristic_realised_contribution_reuse"
)

# --- Injective identity encoding --------------------------------------------------

#' Injective, byte-length-framed encoding of a compound identity
#'
#' Every argument is typed and length-prefixed (and every character element
#' is prefixed by its UTF-8 byte length), so no choice of labels -- delimiters,
#' colons, newlines, U+001F or other control characters -- can make two
#' different tuples encode alike. Use it for every identity built from more
#' than one value (populations, realised map keys, coordinates with labels).
#' Order is significant; callers sort set-valued inputs canonically first.
#'
#' @param ... Values: NULL, character, factor, logical, numeric or lists.
#' @return One character string.
#' @keywords internal
.dsomopFrame <- function(...) {
  encode <- function(v) {
    if (is.null(v)) return("N;")
    if (is.list(v)) {
      inner <- vapply(v, encode, character(1L))
      keys <- names(v)
      if (!is.null(keys)) {
        inner <- paste0(vapply(as.list(keys), encode, character(1L)), inner)
        return(paste0("D", length(v), "{", paste(inner, collapse = ""), "}"))
      }
      return(paste0("L", length(v), "{", paste(inner, collapse = ""), "}"))
    }
    if (is.factor(v)) v <- as.character(v)
    if (inherits(v, "integer64")) v <- as.character(v)
    if (is.character(v)) {
      v <- enc2utf8(v)
      el <- ifelse(is.na(v), "a;",
                   paste0("s", nchar(v, type = "bytes"), ":", v))
      return(paste0("C", length(v), "{", paste(el, collapse = ""), "}"))
    }
    if (is.logical(v)) {
      el <- ifelse(is.na(v), "a;", ifelse(v, "t;", "f;"))
      return(paste0("B", length(v), "{", paste(el, collapse = ""), "}"))
    }
    if (is.numeric(v)) {
      txt <- sprintf("%.17g", v)
      el <- ifelse(is.na(v), "a;",
                   paste0("n", nchar(txt, type = "bytes"), ":", txt))
      return(paste0("R", length(v), "{", paste(el, collapse = ""), "}"))
    }
    stop("An identity element has an unsupported type.", call. = FALSE)
  }
  paste(vapply(list(...), encode, character(1L)), collapse = "")
}

#' Public opaque identifier of one logical release source
#'
#' Derived from the public noise-key fingerprint and the public corpus domain
#' (both already in the DP status), so two connections to one logical node
#' carry the same identifier and a client can refuse to pool them twice. It
#' contains no private corpus material, fingerprint or person data.
#'
#' @param policy DP policy.
#' @return \code{"dsl_"} followed by 40 hexadecimal characters.
#' @keywords internal
.dsomopReleaseLogicalSourceId <- function(policy) {
  paste0("dsl_", substr(.dsomopDpSha256(.dsomopDpCanonicalJson(list(
    protocol = "dsomop-logical-source-v1",
    noise_key_id = policy$noise_root$key_id %||% "",
    domain = policy$domain %||% ""
  ))), 1L, 40L))
}

# --- Public grids ---------------------------------------------------------------

#' Public, versioned release grids
#'
#' Every protected histogram uses one of these fixed grids. A grid's bins are
#' \code{[b_i, b_{i+1})} with a final open bin \code{[b_last, Inf)}; values
#' below the first break are counted in the first bin. \code{lower}/\code{upper}
#' are the public clipping bounds of the bounded moments, or \code{NULL} when
#' the variable has no public scale (its moments are then unavailable).
#'
#' @param id Grid identifier.
#' @return Named list (id, breaks, lower, upper, unit).
#' @keywords internal
.dsomopReleaseGrid <- function(id) {
  grids <- list(
    age_5y = list(id = "age-years-5y-v1", breaks = seq(0, 110, by = 5),
                  lower = 0, upper = 110, unit = "years"),
    duration_days = list(id = "duration-days-v2",
                         breaks = .dsomopReleaseDurationGrid(),
                         lower = 0, upper = 10957, unit = "days"),
    tar_days = list(id = "tar-days-v2",
                    breaks = .DSOMOP_RELEASE_CLINICAL_DAYS,
                    lower = 0, upper = 3650, unit = "days"),
    count_per_person = list(id = "count-per-person-v1",
                            breaks = c(0:20, 25, 30, 40, 50, 75, 100),
                            lower = 0, upper = 100, unit = "count"),
    days_supply = list(id = "days-supply-v1",
                       breaks = c(0, 7, 14, 30, 60, 90, 180, 365),
                       lower = 0, upper = 365, unit = "days"),
    positive_125 = list(id = "positive-1-2-5-v1",
                        breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500,
                                   1000, 2000, 5000, 10000, 20000, 50000,
                                   100000),
                        lower = NULL, upper = NULL, unit = "value"),
    signed_125 = list(id = "signed-1-2-5-v1",
                      breaks = c(-100000, -50000, -20000, -10000, -5000,
                                 -2000, -1000, -500, -200, -100, -50, -20,
                                 -10, -5, -2, -1, 0, 1, 2, 5, 10, 20, 50,
                                 100, 200, 500, 1000, 2000, 5000, 10000,
                                 20000, 50000, 100000),
                      lower = NULL, upper = NULL, unit = "value"),
    score_0_40 = list(id = "integer-score-0-40-v1", breaks = 0:40,
                      lower = 0, upper = 40, unit = "points")
  )
  grid <- grids[[id]]
  if (is.null(grid)) {
    stop("Unknown public release grid '", id, "'.", call. = FALSE)
  }
  grid
}

# Bin labels of a grid, one per bin, in the grid's canonical order. The last
# bin is open above. On a scale without a public lower bound the first bin is
# the explicit underflow bin (-Inf, b2) (N02); on a bounded scale values below
# the first break are clipped into the first bin.
.dsomopReleaseBinFrame <- function(grid) {
  breaks <- grid$breaks
  upper <- c(breaks[-1L], NA_real_)
  lower <- as.numeric(breaks)
  if (is.null(grid$lower) && length(breaks) > 1L) lower[[1L]] <- -Inf
  fmt <- function(x) format(x, scientific = FALSE, trim = TRUE)
  label <- ifelse(is.na(upper),
                  paste0("[", fmt(breaks), ", Inf)"),
                  paste0("[", fmt(breaks), ", ", fmt(upper), ")"))
  if (is.infinite(lower[[1L]])) {
    label[[1L]] <- paste0("(-Inf, ", fmt(upper[[1L]]), ")")
  }
  data.frame(bin = seq_along(breaks), bin_label = label,
             bin_lower = lower, bin_upper = as.numeric(upper),
             stringsAsFactors = FALSE)
}

# Canonical coordinate key of each bin: the public grid id and its interval.
.dsomopReleaseBinKeys <- function(grid) {
  paste0(grid$id, ":", .dsomopReleaseBinFrame(grid)$bin_label)
}

# Bin index of each value: values below the first break go to bin 1, the last
# bin is open above.
.dsomopReleaseBinIndex <- function(values, grid) {
  breaks <- grid$breaks
  index <- findInterval(values, breaks)
  index[!is.na(index) & index < 1L] <- 1L
  index
}

# --- Public quantization ------------------------------------------------------

#' Snap a value to the nearest point of a sorted public grid
#'
#' \code{snap_G(x) = argmin_g (|x - g|, g)}: ties go to the smaller grid point
#' and values outside the grid saturate at its endpoints. Evaluated with exact
#' integer arithmetic when every grid point and the value are integers.
#'
#' @param x Numeric scalar.
#' @param grid Sorted numeric vector.
#' @return The snapped grid value.
#' @keywords internal
.dsomopReleaseSnap <- function(x, grid) {
  grid <- sort(unique(as.numeric(grid)))
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
    stop("A public release parameter must be one finite number.",
         call. = FALSE)
  }
  if (x <= grid[[1L]]) return(grid[[1L]])
  if (x >= grid[[length(grid)]]) return(grid[[length(grid)]])
  distance <- abs(grid - x)
  best <- which(distance == min(distance))
  grid[[min(best)]]
}

# Largest grid point <= x (saturating), and smallest grid point >= x.
.dsomopReleaseSnapFloor <- function(x, grid) {
  grid <- sort(unique(as.numeric(grid)))
  below <- grid[grid <= x]
  if (length(below) == 0L) grid[[1L]] else below[[length(below)]]
}

.dsomopReleaseSnapCeil <- function(x, grid) {
  grid <- sort(unique(as.numeric(grid)))
  above <- grid[grid >= x]
  if (length(above) == 0L) grid[[length(grid)]] else above[[1L]]
}

# Public clinical day anchors (dsOMOP 2.10.3): common acute and long-term
# windows and durations are fixed points of every day grid, so standard
# clinical requests keep their exact closed-day meaning.
.DSOMOP_RELEASE_CLINICAL_DAYS <- c(0, 1, 7, 14, 21, 28, 30, 60, 90, 120, 180,
                                   270, 365, 545, 730, 1095, 1460, 1825,
                                   3650)

# G: the signed offset grid, every multiple of 30 days with |z| <= 121 plus
# the clinical anchors and their negatives.
.dsomopReleaseSignedDayGrid <- function() {
  C <- .DSOMOP_RELEASE_CLINICAL_DAYS
  sort(unique(c(30 * seq.int(-121L, 121L), C, -C)))
}

# L: the non-negative look-back / follow-up duration grid, G within [0, 3650].
.dsomopReleaseDayGrid <- function() {
  G <- .dsomopReleaseSignedDayGrid()
  G[G >= 0 & G <= 3650]
}

# D: the general duration histogram / threshold grid with its declared tails.
.dsomopReleaseDurationGrid <- function() {
  sort(unique(c(.DSOMOP_RELEASE_CLINICAL_DAYS, 7300, 10957)))
}

.dsomopReleaseProbabilityGrid <- function() {
  round(seq(0.01, 0.99, by = 0.01), 2)
}

.dsomopReleaseAgeGrid <- function() seq(0, 110, by = 5)

# Public classification of every free parameter of a built-in aggregate entry.
# A parameter is "computation" (it selects the population or the computation
# and snaps to a public grid), "effective_release" (it changes only which view
# of protected components is shown) or "presentation". Each class names its
# quantizer.
.DSOMOP_RELEASE_PARAM_CLASSES <- list(
  tar_start_offset = list(class = "computation", quantizer = "window_start"),
  tar_end_offset = list(class = "computation", quantizer = "window_end"),
  covariate_start_offset = list(class = "computation",
                                quantizer = "window_start"),
  covariate_end_offset = list(class = "computation",
                              quantizer = "window_end"),
  temporal_start_days = list(class = "computation",
                             quantizer = "window_start"),
  temporal_end_days = list(class = "computation", quantizer = "window_end"),
  window = list(class = "computation", quantizer = "day_duration"),
  pre_window = list(class = "computation", quantizer = "day_duration"),
  bin_width = list(class = "computation", quantizer = "day_duration_positive"),
  min_prior_observation = list(class = "computation",
                               quantizer = "day_duration"),
  clean_window = list(class = "computation", quantizer = "day_duration"),
  min_observed_days = list(class = "computation",
                           quantizer = "day_duration_positive"),
  dechallenge_stop_interval = list(class = "computation",
                                   quantizer = "day_duration_positive"),
  dechallenge_evaluation_window = list(class = "computation",
                                       quantizer = "day_duration_positive"),
  min_days = list(class = "effective_release", quantizer = "duration_grid"),
  min_age = list(class = "computation", quantizer = "age_lower"),
  max_age = list(class = "computation", quantizer = "age_upper"),
  age_band_years = list(class = "computation", quantizer = "age_band"),
  age_breaks = list(class = "computation", quantizer = "age_breaks"),
  reference_year = list(class = "effective_release",
                        quantizer = "reference_year"),
  alpha = list(class = "effective_release", quantizer = "probability"),
  power = list(class = "effective_release", quantizer = "probability"),
  ease_smd_threshold = list(class = "presentation",
                            quantizer = "released_threshold"),
  min_smd = list(class = "presentation", quantizer = "released_threshold"),
  min_mean = list(class = "presentation", quantizer = "released_threshold"),
  min_count = list(class = "presentation", quantizer = "display_integer"),
  top_n = list(class = "presentation", quantizer = "display_integer"),
  top_n_condition = list(class = "presentation",
                         quantizer = "display_integer"),
  top_n_drug = list(class = "presentation", quantizer = "display_integer"),
  max_path_length = list(class = "computation", quantizer = "declared_range"),
  time_windows = list(class = "computation", quantizer = "declared_range"),
  top_k = list(class = "computation", quantizer = "model_features"),
  max_covariates = list(class = "computation", quantizer = "model_features"),
  n_bins = list(class = "computation", quantizer = "calibration_bins"),
  concept_id = list(class = "computation", quantizer = "discrete"),
  visit_concept_id = list(class = "computation", quantizer = "discrete")
)

#' Classification of one free parameter of an aggregate entry
#'
#' Enum, Boolean and concept-id parameters are discrete public domains. Every
#' other numeric parameter must be classified in
#' \code{.DSOMOP_RELEASE_PARAM_CLASSES}; an unclassified numeric parameter is a
#' contract violation (it fails at catalogue validation, never silently).
#'
#' @param spec Parameter spec of the entry.
#' @return Classification list, or \code{NULL} for discrete parameters.
#' @keywords internal
.dsomopReleaseParamClass <- function(spec) {
  type <- spec$type %||% ""
  if (type %in% c("enum", "bool", "concept_id")) {
    return(list(class = "computation", quantizer = "discrete"))
  }
  .DSOMOP_RELEASE_PARAM_CLASSES[[spec$name]]
}

.dsomopReleaseIntegerText <- function(x) {
  format(as.numeric(x), scientific = FALSE, trim = TRUE)
}

#' Snap the free numeric parameters of an aggregate request to public grids
#'
#' Applies the public quantizer of every classified parameter before the
#' final aggregate is evaluated and before its identity is formed, so a request
#' evaluates exactly the effective estimand it reports. Closed integer windows
#' \code{[a, b]} snap outward to \code{[floor_G(a), ceil_G(b)]} on the signed
#' clinical grid G (and are converted once to the half-open
#' \code{[a, b + 1)} in SQL), so common clinical windows such as
#' \code{[1, 365]} or \code{[0, 0]} are unchanged; scalar durations snap to the
#' nearest point of the duration grids (ties to the smaller one) with endpoint
#' saturation.
#' Discrete, display and released-value thresholds are unchanged. The returned
#' \code{changes} name every parameter whose effective value differs from the
#' request, with the requested and effective values.
#'
#' @param entry Catalog entry.
#' @param sanitized Sanitized parameter list (string literals).
#' @return List with \code{effective} (sanitized-form parameters) and
#'   \code{changes} (named list of requested/effective/rule).
#' @keywords internal
.omopToolsSnapReleaseParams <- function(entry, sanitized) {
  sanitized <- sanitized %||% list()
  specs <- entry$params %||% list()
  by_name <- stats::setNames(specs, vapply(specs, function(p) p$name,
                                           character(1L)))
  effective <- sanitized
  changes <- list()
  number <- function(value) suppressWarnings(as.numeric(value))
  record <- function(name, requested, value, rule) {
    if (!identical(as.character(requested), as.character(value))) {
      changes[[name]] <<- list(requested = requested, effective = value,
                               rule = rule)
    }
    effective[[name]] <<- value
  }
  for (name in names(sanitized)) {
    spec <- by_name[[name]]
    if (is.null(spec)) next
    cls <- .dsomopReleaseParamClass(spec)
    if (is.null(cls)) next
    value <- sanitized[[name]]
    q <- cls$quantizer
    if (q %in% c("discrete", "display_integer", "declared_range",
                 "released_threshold", "model_features", "calibration_bins")) {
      next
    }
    x <- number(value)
    if (identical(q, "age_breaks")) {
      breaks <- as.numeric(strsplit(as.character(value), ",", fixed = TRUE)[[1L]])
      snapped <- unique(vapply(breaks, .dsomopReleaseSnap, numeric(1L),
                               grid = .dsomopReleaseAgeGrid()))
      snapped <- sort(snapped[snapped > 0 & snapped < 110])
      if (length(snapped) == 0L) {
        stop("Analysis '", entry$name, "': age_breaks have no interior ",
             "boundary on the public 5-year age grid.", call. = FALSE)
      }
      record(name, value, paste(snapped, collapse = ","),
             "age_breaks_public_5y_grid")
      next
    }
    if (length(x) != 1L || is.na(x)) next
    # Closed integer windows [a, b] snap to [floor_G(a), ceil_G(b)] (converted
    # once to the half-open [a, b + 1) in SQL); scalar durations snap to the
    # nearest point of L (strictly positive ones to L without 0), thresholds
    # to D, ties to the smaller point, finite values saturating.
    out <- switch(q,
      window_start = .dsomopReleaseSnapFloor(x, .dsomopReleaseSignedDayGrid()),
      window_end = .dsomopReleaseSnapCeil(x, .dsomopReleaseSignedDayGrid()),
      day_duration = .dsomopReleaseSnap(max(x, 0), .dsomopReleaseDayGrid()),
      day_duration_positive = .dsomopReleaseSnap(
        max(x, 0), setdiff(.dsomopReleaseDayGrid(), 0)),
      duration_grid = .dsomopReleaseSnap(x, .dsomopReleaseDurationGrid()),
      age_lower = .dsomopReleaseSnapFloor(x, .dsomopReleaseAgeGrid()),
      age_upper = .dsomopReleaseSnapCeil(x + 1, .dsomopReleaseAgeGrid()) - 1,
      age_band = .dsomopReleaseSnap(x, c(5, 10, 20)),
      reference_year = .dsomopToolsReferenceYear(),
      probability = .dsomopReleaseSnap(x, .dsomopReleaseProbabilityGrid()),
      x
    )
    text <- if (identical(q, "probability")) {
      format(out, nsmall = 2, trim = TRUE)
    } else .dsomopReleaseIntegerText(out)
    record(name, value, text, q)
  }
  list(effective = effective, changes = changes)
}

# --- Keys, person tokens and contribution fingerprints ------------------------

#' Domain-separated secret subkeys of the protected-release mechanism
#'
#' Derived from the existing noise root with HMAC-SHA256 labels, separately for
#' person pseudonyms, contribution fingerprints and noise. Never exported.
#'
#' @param policy DP policy from \code{.dsomopDpPolicy()}.
#' @return List of raw 32-byte keys.
#' @keywords internal
.dsomopReleaseKeys <- function(policy) {
  root <- policy$noise_root$key
  if (!is.raw(root) || length(root) != 32L) {
    stop(structure(list(message = .DSOMOP_DP_STATE_ERROR, call = NULL),
                   class = c("dsomop_dp_state_error", "error", "condition")))
  }
  list(
    person = .dsomopDpSubkey(root, policy$domain, "release-person-token/v1"),
    fingerprint = .dsomopDpSubkey(root, policy$domain,
                                  "release-contribution-fingerprint/v1"),
    noise = .dsomopDpSubkey(root, policy$domain, "release-noise/v1")
  )
}

# Exact canonical text of person identifiers (integers never use scientific or
# rounded floating-point notation).
.dsomopReleasePersonIdText <- function(ids) {
  if (inherits(ids, "integer64")) return(as.character(ids))
  if (is.numeric(ids)) {
    if (any(!is.finite(ids[!is.na(ids)])) ||
        any(ids[!is.na(ids)] != floor(ids[!is.na(ids)]))) {
      stop("Person identifiers must be integers.", call. = FALSE)
    }
    out <- sprintf("%.0f", ids)
    out[is.na(ids)] <- NA_character_
    return(out)
  }
  out <- enc2utf8(trimws(as.character(ids)))
  out[is.na(ids)] <- NA_character_
  out
}

#' Keyed person tokens of the protected-release mechanism
#'
#' \code{HMAC(K_person, frame(corpus, person_id))}: collision resistant,
#' stable across sessions, handles and workers sharing the noise root, and never
#' exported.
#'
#' @param keys From \code{.dsomopReleaseKeys}.
#' @param policy DP policy (supplies the corpus).
#' @param ids Raw CDM person identifiers.
#' @return Character vector of 64-character hex tokens.
#' @keywords internal
.dsomopReleasePersonTokens <- function(keys, policy, ids) {
  text <- .dsomopReleasePersonIdText(ids)
  if (anyNA(text) || any(!nzchar(text))) {
    stop("Protected releases need a person identifier for every row.",
         call. = FALSE)
  }
  # Byte-length framed (B04): no identifier or domain text can shift a field
  # boundary.
  domain <- enc2utf8(as.character(policy$domain %||% ""))
  text <- enc2utf8(text)
  framed <- paste0("dsomop-release-person-v2|", nchar(domain, type = "bytes"),
                   ":", domain, "|", nchar(text, type = "bytes"), ":", text)
  as.character(openssl::sha256(framed, key = keys$person))
}

.dsomopReleaseIntegerString <- function(x) {
  if (any(!is.finite(x)) || any(x != floor(x)) || any(abs(x) > 2^53)) {
    stop("A bounded contribution is not an exact integer.", call. = FALSE)
  }
  sprintf("%.0f", x)
}

#' Secret fingerprint of the realised bounded contributions of one component
#'
#' \code{F = HMAC(K_fingerprint, frame('contributions-v1', owner, family,
#' coordinate, sorted length-framed (person_token, contribution)))}. The map
#' is sorted by token, so row order, duplicated source rows removed by the
#' person reducer, scope names and lineage spellings never change it; a changed
#' bounded contribution always does. Empty components keep their distinct
#' coordinate identity.
#'
#' @param keys Release keys.
#' @param owner,family,coordinate Public component identity.
#' @param tokens Person tokens of the contributors.
#' @param contributions Exact integer contributions (same length).
#' @return Hex fingerprint (never exported).
#' @keywords internal
.dsomopReleaseFingerprint <- function(keys, owner, family, coordinate,
                                      tokens = character(0),
                                      contributions = numeric(0)) {
  if (length(tokens) != length(contributions)) {
    stop("A contribution map is internally inconsistent.", call. = FALSE)
  }
  if (anyDuplicated(tokens)) {
    stop("A contribution map lists one person twice.", call. = FALSE)
  }
  digests <- character(0)
  if (length(tokens) > 0L) {
    order <- order(tokens, method = "radix")
    tok <- enc2utf8(as.character(tokens[order]))
    # Length-prefixed tokens (B04): a record decodes unambiguously whatever
    # characters a token holds.
    records <- paste0(nchar(tok, type = "bytes"), ":", tok, ":",
                      .dsomopReleaseIntegerString(contributions[order]))
    chunks <- split(records, ceiling(seq_along(records) / 10000L))
    digests <- vapply(chunks, function(chunk) {
      .dsomopDpHmac(keys$fingerprint, paste(
        c("dsomop-contribution-chunk-v2", length(chunk), chunk),
        collapse = "\n"))
    }, character(1L), USE.NAMES = FALSE)
  }
  .dsomopDpHmac(keys$fingerprint, .dsomopDpCanonicalJson(list(
    protocol = "dsomop-contributions-v1", owner = owner, family = family,
    coordinate = coordinate, n = length(tokens),
    chunks = paste(digests, collapse = ",")
  )))
}

# --- Noisy components ------------------------------------------------------------

#' Fixed noise profile of a component
#'
#' Mechanism, sampler, lattice, allocation and sensitivity versions. It never
#' contains request text, requested caps, output lists, sessions or handles.
#' @keywords internal
.dsomopReleaseProfile <- function(policy, allocation, sensitivity) {
  list(
    protocol = .DSOMOP_RELEASE_PROTOCOL,
    mechanism = policy$mechanism,
    sampler = policy$sampler,
    reference_epsilon = policy$release_epsilon,
    allocation = allocation,
    sensitivity = sensitivity,
    lattice = .DSOMOP_RELEASE_LATTICE,
    privacy_epoch = policy$privacy_epoch
  )
}

#' Draw the signed noisy value of one protected component
#'
#' Exact integer addition of the discrete-Laplace noise; no clipping (the
#' signed component is kept for pooling and projected only for display).
#' \code{fixed_synopsis} keys the noise on (corpus, custodial generation,
#' owner, population, family, coordinate); \code{realised_sticky} keys it on
#' the owner, family, coordinate and the secret contribution fingerprint.
#'
#' @param policy DP policy.
#' @param keys Release keys.
#' @param protection "fixed_synopsis" or "realised_sticky".
#' @param component List(owner, population, family, coordinate, value,
#'   allocation, sensitivity, fingerprint).
#' @return Numeric signed noisy value.
#' @keywords internal
.dsomopReleaseNoisyComponent <- function(policy, keys, protection, component) {
  value <- component$value
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      value != floor(value) || abs(value) > 2^53 - 1) {
    stop("A protected component is not exactly representable.", call. = FALSE)
  }
  epsilon <- policy$release_epsilon * component$allocation
  profile <- .dsomopReleaseProfile(policy, component$allocation,
                                   component$sensitivity)
  context <- if (identical(protection, "fixed_synopsis")) {
    list(protocol = "dsomop-fixed-basis-v1", corpus = policy$domain,
         generation = list(snapshot = policy$snapshot_id,
                           epoch = policy$privacy_epoch),
         owner = component$owner, population = component$population,
         family = component$family, coordinate = component$coordinate,
         profile = profile)
  } else if (identical(protection, "realised_sticky")) {
    if (!is.character(component$fingerprint) ||
        length(component$fingerprint) != 1L ||
        !grepl("^[0-9a-f]{64}$", component$fingerprint)) {
      stop("A realised component lacks its contribution fingerprint.",
           call. = FALSE)
    }
    list(protocol = "dsomop-realised-sticky-v1", corpus = policy$domain,
         owner = component$owner, family = component$family,
         coordinate = component$coordinate, profile = profile,
         contributions = component$fingerprint)
  } else {
    stop("Unknown protected release route.", call. = FALSE)
  }
  noise <- .dsomopDpDiscreteLaplace(keys$noise, context, coordinate = 1L,
                                    epsilon = epsilon,
                                    sensitivity = component$sensitivity)
  .dsomopBigNumeric(.dsomopBigAdd(value, noise))
}

# Standard deviation of the discrete-Laplace noise of a component (public).
.dsomopReleaseNoiseSd <- function(epsilon, sensitivity) {
  a <- exp(-epsilon / sensitivity)
  sqrt(2 * a) / (1 - a)
}

# --- Deterministic projections -------------------------------------------------

#' Euclidean projection onto the simplex \code{{h >= 0, sum(h) = total}}
#'
#' Deterministic: values are processed in a stable order (decreasing value,
#' then the canonical index), so ties never depend on data order.
#'
#' @param y Numeric vector (signed noisy cells, canonical order).
#' @param total Non-negative target total.
#' @return Numeric vector of the same length.
#' @keywords internal
.dsomopReleaseSimplex <- function(y, total) {
  y <- as.numeric(y)
  n <- length(y)
  if (n == 0L) return(numeric(0))
  total <- max(0, as.numeric(total))
  if (total == 0) return(rep(0, n))
  order <- order(-y, seq_len(n), method = "radix")
  sorted <- y[order]
  cumulative <- cumsum(sorted)
  k <- seq_len(n)
  candidate <- sorted - (cumulative - total) / k
  rho <- max(which(candidate > 0))
  theta <- (cumulative[[rho]] - total) / rho
  pmax(y - theta, 0)
}

#' Project noisy normalized moments onto their feasible set
#'
#' Minimizes \code{(S - S0)^2 + (Q - Q0)^2} subject to \code{|S| <= N},
#' \code{0 <= Q <= N} and \code{S^2 <= N Q} for a fixed displayed count
#' \code{N}. The set is closed and convex, so the projection is unique; it is
#' found among the boundary candidates (cap line, corners, parabola) with a
#' fixed-iteration bisection. No raw statistic enters.
#'
#' @param S0,Q0 Noisy normalized sum and square sum.
#' @param N Displayed (non-negative) count.
#' @return Named numeric c(S, Q).
#' @keywords internal
.dsomopReleaseProjectMoments <- function(S0, Q0, N) {
  if (!is.finite(N) || N <= 0) return(c(S = 0, Q = 0))
  feasible <- function(S, Q) {
    Q >= 0 && Q <= N + 1e-12 * N && S * S <= N * Q + 1e-9 * N
  }
  if (feasible(S0, Q0)) return(c(S = S0, Q = Q0))
  candidates <- list(c(-N, N), c(N, N))
  if (abs(S0) <= N) candidates[[length(candidates) + 1L]] <- c(S0, N)
  # Nearest point on the parabola Q = S^2 / N with |S| <= N: the stationary
  # equation g(S) = (S - S0) + 2 S (S^2 / N - Q0) / N changes sign between 0
  # and S0 when the point lies below the parabola.
  g <- function(S) (S - S0) + 2 * S * (S * S / N - Q0) / N
  lo <- min(0, S0)
  hi <- max(0, S0)
  if (g(lo) * g(hi) <= 0) {
    for (i in seq_len(200L)) {
      mid <- (lo + hi) / 2
      if (g(lo) * g(mid) <= 0) hi <- mid else lo <- mid
    }
    s <- max(-N, min(N, (lo + hi) / 2))
    candidates[[length(candidates) + 1L]] <- c(s, s * s / N)
  }
  candidates[[length(candidates) + 1L]] <- c(0, 0)
  distance <- vapply(candidates, function(p) (p[[1L]] - S0)^2 +
                       (p[[2L]] - Q0)^2, numeric(1L))
  ok <- vapply(candidates, function(p) feasible(p[[1L]], p[[2L]]),
               logical(1L))
  best <- which(ok)[which.min(distance[ok])]
  point <- candidates[[best]]
  c(S = point[[1L]], Q = max(0, min(N, point[[2L]])))
}

# Fixed canonical rounding of derived outputs.
.dsomopReleaseRound <- function(x) signif(as.numeric(x), 12)

#' Quantiles of a displayed histogram by monotone CDF inversion
#'
#' Linear interpolation inside finite bins; a quantile falling in the open last
#' bin reports that bin's lower edge.
#'
#' @param counts Non-negative displayed bin counts.
#' @param grid Public grid.
#' @param probs Probabilities (snapped to 0.01 steps).
#' @return Numeric vector of quantiles (NA when the histogram is empty).
#' @keywords internal
.dsomopReleaseQuantiles <- function(counts, grid,
                                    probs = c(0.1, 0.25, 0.5, 0.75, 0.9)) {
  probs <- round(probs, 2)
  total <- sum(counts)
  if (!is.finite(total) || total <= 0) return(rep(NA_real_, length(probs)))
  breaks <- grid$breaks
  upper <- c(breaks[-1L], NA_real_)
  open_below <- is.null(grid$lower) && length(breaks) > 1L
  cdf <- cumsum(counts) / total
  vapply(probs, function(p) {
    bin <- which(cdf >= p - 1e-12)[1L]
    before <- if (bin > 1L) cdf[[bin - 1L]] else 0
    width <- cdf[[bin]] - before
    # Open tail bins report their finite edge (a censored quantile, never a
    # pretended extreme).
    if (is.na(upper[[bin]])) return(breaks[[bin]])
    if (bin == 1L && open_below) return(upper[[1L]])
    fraction <- if (width > 0) (p - before) / width else 0
    breaks[[bin]] + fraction * (upper[[bin]] - breaks[[bin]])
  }, numeric(1L))
}

# --- Owners, components and displayed views ----------------------------------

#' Declare one protected component owner
#'
#' An owner is one reviewed estimand family: a person reducer and population
#' (\code{population}), a public stratification (\code{strata}), and either a
#' value grid with optional public moment bounds or a complete public cell
#' domain. \code{family} is "H" (histogram/partition), "HM" (histogram and
#' bounded moments) or "M" (bounded moments). Every person contributes to
#' \code{cap} strata at most (1 for a partition; the multi-concept cap K,
#' ascending stable public stratum order, for data-dependent strata), so the
#' vector sensitivity is \code{cap} in counts and \code{cap * J} in lattice
#' moments.
#'
#' @param id Unique public estimand identifier (never reused for another
#'   estimand).
#' @param population Public population-basis identifier (defaults to the
#'   owner id).
#' @param family "H", "HM" or "M".
#' @param grid Public grid (value owners) or NULL.
#' @param cells Complete public cell keys (cell owners) or NULL.
#' @param cell_labels Optional data frame of public labels per cell key.
#' @param strata "root", or list(kind = "public", keys, labels) or
#'   list(kind = "data").
#' @param cap Contribution cap per person.
#' @param bounds Public moment bounds c(L, U) or NULL.
#' @return Owner spec.
#' @keywords internal
.dsomopReleaseOwner <- function(id, population = NULL, family = "H", grid = NULL,
                                cells = NULL, cell_labels = NULL,
                                strata = "root", cap = 1L, bounds = NULL,
                                estimand = NULL) {
  if (!family %in% c("H", "HM", "M")) stop("Unknown owner family.",
                                            call. = FALSE)
  if (identical(strata, "root")) {
    strata <- list(kind = "root", keys = "root",
                   labels = data.frame(stratum = "root",
                                       stringsAsFactors = FALSE))
  }
  if (!is.null(bounds)) {
    bounds <- as.numeric(bounds)
    if (length(bounds) != 2L || !all(is.finite(bounds)) ||
        bounds[[1L]] >= bounds[[2L]]) {
      stop("Public moment bounds must be finite with lower < upper.",
           call. = FALSE)
    }
  }
  # In the fixed-basis route a population id keys its person totals, so it
  # defaults to the owner id: two owners never share fixed noise unless they
  # declare the very same population definition.
  population <- population %||% id
  list(id = id, population = population, family = family, grid = grid,
       cells = cells, cell_labels = cell_labels, strata = strata,
       cap = as.integer(cap), bounds = bounds,
       estimand = estimand %||% id)
}

# Disclosure settings used by protected releases.
.dsomopReleaseSupport <- function() {
  s <- .omopDisclosureSettings()
  list(floor = max(s$nfilter_tab, s$nfilter_subset),
       dist = s$nfilter_dist %||% 10L)
}

# Generic analyst-facing disclosure error (no counts).
.dsomopReleaseDisclosive <- function() {
  stop("Disclosive: operation blocked - insufficient individuals to meet ",
       "disclosure threshold. No further details available.", call. = FALSE)
}

#' Apply the contribution cap to a person-level substrate
#'
#' Partitions (cap 1 over public strata) must already have one row per person;
#' data-dependent strata keep each person's first \code{cap} strata in
#' ascending public order (numeric \code{stratum_order}, then key). The cap is
#' applied to the complete expansion before any display selection.
#' @keywords internal
.dsomopReleaseCapSubstrate <- function(sub, owner) {
  if (nrow(sub) == 0L) return(sub)
  key <- paste(sub$person_id, sub$stratum, sep = "\u001f")
  if (anyDuplicated(key)) {
    stop("A protected substrate lists one person twice in one stratum.",
         call. = FALSE)
  }
  if (!identical(owner$strata$kind, "data")) {
    if (anyDuplicated(sub$person_id)) {
      stop("A protected partition lists one person in two strata.",
           call. = FALSE)
    }
    return(sub)
  }
  order_value <- suppressWarnings(as.numeric(sub$stratum_order %||%
                                               rep(NA_real_, nrow(sub))))
  ord <- order(sub$person_id, order_value, sub$stratum, method = "radix",
               na.last = TRUE)
  sub <- sub[ord, , drop = FALSE]
  rank <- stats::ave(seq_len(nrow(sub)), sub$person_id, FUN = seq_along)
  sub[rank <= owner$cap, , drop = FALSE]
}

#' Integer lattice contributions of bounded values
#'
#' Each value is clipped to the public bounds \code{[L, U]}, normalized to
#' \code{z = (2x - L - U) / (U - L)} in \code{[-1, 1]} and contributes
#' \code{round(J z)} to S and \code{round(J z^2)} to Q (J = 65535), so one
#' person changes S and Q by at most J each.
#' @keywords internal
.dsomopReleaseLattice <- function(v, bounds) {
  J <- .DSOMOP_RELEASE_LATTICE
  L <- bounds[[1L]]; U <- bounds[[2L]]
  x <- pmin(U, pmax(L, as.numeric(v)))
  z <- (2 * x - L - U) / (U - L)
  list(s = round(J * z), q = round(J * z * z))
}

#' Exact bounded components of one owner (private; for sensitivity checks)
#'
#' The same reducers, caps, binning and lattice as the release builder,
#' without noise. Never released.
#' @keywords internal
.dsomopReleaseExactComponents <- function(owner, sub) {
  sub <- sub[!is.na(sub$person_id) & !is.na(sub$stratum), , drop = FALSE]
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  if (is.null(owner$cells)) {
    sub$v <- suppressWarnings(as.numeric(sub$v))
    sub <- sub[is.finite(sub$v), , drop = FALSE]
  }
  sub <- .dsomopReleaseCapSubstrate(sub, owner)
  keys <- if (identical(owner$strata$kind, "data")) sort(unique(sub$stratum))
    else owner$strata$keys
  nbins <- if (!is.null(owner$cells)) length(owner$cells) else
    if (!is.null(owner$grid)) length(owner$grid$breaks) else 0L
  out <- lapply(keys, function(s) {
    in_s <- sub$stratum == s
    h <- if (!is.null(owner$cells)) {
      tabulate(match(sub$cell[in_s], owner$cells), nbins)
    } else if (!is.null(owner$grid)) {
      tabulate(.dsomopReleaseBinIndex(sub$v[in_s], owner$grid), nbins)
    } else numeric(0)
    m <- if (!is.null(owner$bounds)) {
      .dsomopReleaseLattice(sub$v[in_s], owner$bounds)
    } else list(s = 0, q = 0)
    list(n = sum(in_s), h = as.numeric(h), s = sum(m$s), q = sum(m$q))
  })
  stats::setNames(out, keys)
}

#' Build, noise and display the protected components of one owner
#'
#' @param policy DP policy.
#' @param protection "fixed_synopsis" or "realised_sticky".
#' @param owner Owner spec.
#' @param sub Person-level substrate: \code{person_id}, \code{stratum} (public
#'   stratum key; "root" when unstratified), and \code{v} (bounded value) or
#'   \code{cell} (public cell key); optional \code{stratum_order}.
#' @param data_strata_keys For data-dependent strata, the stratum keys to
#'   release (after support admission); NULL selects every supported stratum.
#' @param n_only Draw only the person totals (a first pass that ranks
#'   data-dependent strata on released values; the noise is deterministic, so a
#'   second full pass reproduces the same totals).
#' @return List: \code{strata} (data frame of displayed per-stratum results),
#'   \code{bins} (displayed histogram per stratum), \code{export} (signed
#'   components for pooling) and \code{meta}.
#' @keywords internal
.dsomopReleaseBuild <- function(policy, protection, owner, sub,
                                data_strata_keys = NULL, n_only = FALSE) {
  keys <- .dsomopReleaseKeys(policy)
  support <- .dsomopReleaseSupport()
  J <- .DSOMOP_RELEASE_LATTICE
  needed <- c("person_id", "stratum")
  if (!is.data.frame(sub) || !all(needed %in% names(sub))) {
    stop("A protected substrate lacks person/stratum columns.", call. = FALSE)
  }
  is_cell <- !is.null(owner$cells)
  if (is_cell && !"cell" %in% names(sub)) {
    stop("A protected partition substrate lacks its cell column.",
         call. = FALSE)
  }
  if (!is_cell && !"v" %in% names(sub)) {
    stop("A protected value substrate lacks its value column.", call. = FALSE)
  }
  sub <- sub[!is.na(sub$person_id) & !is.na(sub$stratum), , drop = FALSE]
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  sub$stratum <- as.character(sub$stratum)
  if (is_cell) {
    sub$cell <- as.character(sub$cell)
    if (anyNA(sub$cell) || any(!sub$cell %in% owner$cells)) {
      stop("A protected partition substrate has a cell outside its public ",
           "domain.", call. = FALSE)
    }
  } else {
    sub$v <- suppressWarnings(as.numeric(sub$v))
    sub <- sub[is.finite(sub$v), , drop = FALSE]
  }
  if (!identical(owner$strata$kind, "data")) {
    unknown <- setdiff(unique(sub$stratum), owner$strata$keys)
    if (length(unknown)) {
      stop("A protected substrate has a stratum outside its public domain.",
           call. = FALSE)
    }
  }
  sub <- .dsomopReleaseCapSubstrate(sub, owner)
  # Exact eligible-population support (the parent basis), checked before any
  # release; failures use the generic disclosure error.
  n_total <- length(unique(sub$person_id))
  if (n_total < support$floor) .dsomopReleaseDisclosive()
  tokens <- if (nrow(sub)) .dsomopReleasePersonTokens(keys, policy,
                                                      sub$person_id) else
    character(0)
  cap <- max(1L, owner$cap)
  frac <- .DSOMOP_RELEASE_FRACTIONS
  alloc_h <- switch(owner$family, H = frac$H$H, HM = frac$HM$H, M = NA_real_)
  alloc_s <- switch(owner$family, H = NA_real_, HM = frac$HM$S, M = frac$M$S)
  alloc_q <- switch(owner$family, H = NA_real_, HM = frac$HM$Q, M = frac$M$Q)
  has_h <- owner$family %in% c("H", "HM") && !isTRUE(n_only)
  has_m <- owner$family %in% c("HM", "M") && !is.null(owner$bounds) &&
    !isTRUE(n_only)

  # Strata to release.
  exact_n <- table(factor(sub$stratum))
  if (identical(owner$strata$kind, "data")) {
    supported <- names(exact_n)[as.numeric(exact_n) >= support$floor]
    strata_keys <- if (is.null(data_strata_keys)) supported else
      intersect(as.character(data_strata_keys), supported)
    order_value <- suppressWarnings(as.numeric(
      sub$stratum_order[match(strata_keys, sub$stratum)] %||% NA_real_))
    strata_keys <- strata_keys[order(order_value, strata_keys,
                                     method = "radix", na.last = TRUE)]
  } else {
    strata_keys <- owner$strata$keys
  }
  bin_keys <- if (is_cell) owner$cells else if (has_h)
    .dsomopReleaseBinKeys(owner$grid) else character(0)

  noisy <- function(family, coordinate, value, allocation, sensitivity,
                    map_tokens, contributions, owner_id = owner$id,
                    population = owner$population) {
    fp <- if (identical(protection, "realised_sticky")) {
      .dsomopReleaseFingerprint(keys, owner_id, family, coordinate,
                                map_tokens, contributions)
    } else NULL
    .dsomopReleaseNoisyComponent(policy, keys, protection, list(
      owner = owner_id, population = population, family = family,
      coordinate = coordinate, value = value, allocation = allocation,
      sensitivity = sensitivity, fingerprint = fp))
  }

  # Summary statistics (moments, quantiles) need nfilter.dist distinct
  # persons: per released row for data-dependent strata, on the parent
  # eligible population for a complete public partition (whose cells are
  # never re-tested one by one).
  data_kind <- identical(owner$strata$kind, "data")
  parent_dist_ok <- n_total >= support$dist
  # The public marginal hierarchy: one canonical parent person total of the
  # owner's eligible population (shared by every owner that declares the same
  # population), drawn before any child stratification. Children of a public
  # partition are reconciled to it and projected onto its displayed total; a
  # child request never moves the parent.
  public_kind <- identical(owner$strata$kind, "public") &&
    length(strata_keys) > 1L
  root_noisy <- NA_real_
  if (!data_kind) {
    first <- !duplicated(sub$person_id)
    root_noisy <- noisy("N", "stratum=root", n_total, frac$N, cap,
                        tokens[first], rep(1, sum(first)),
                        owner_id = .DSOMOP_RELEASE_N_OWNER)
  }
  rows <- vector("list", length(strata_keys))
  bins <- vector("list", length(strata_keys))
  for (i in seq_along(strata_keys)) {
    s <- strata_keys[[i]]
    in_s <- sub$stratum == s
    tok <- tokens[in_s]
    n_exact <- sum(in_s)
    dist_ok <- if (data_kind) n_exact >= support$dist else parent_dist_ok
    n_noisy <- if (!data_kind && !public_kind) root_noisy else
      noisy("N", paste0("stratum=", s), n_exact, frac$N, cap, tok,
            rep(1, n_exact), owner_id = .DSOMOP_RELEASE_N_OWNER)
    h_noisy <- rep(NA_real_, length(bin_keys))
    if (has_h && (dist_ok || !data_kind)) {
      cell <- if (is_cell) match(sub$cell[in_s], owner$cells) else
        .dsomopReleaseBinIndex(sub$v[in_s], owner$grid)
      h_noisy <- vapply(seq_along(bin_keys), function(b) {
        hit <- which(cell == b)
        noisy("H", paste0("stratum=", s, "|cell=", bin_keys[[b]]),
              length(hit), alloc_h, cap, tok[hit], rep(1, length(hit)))
      }, numeric(1L))
    }
    s_noisy <- q_noisy <- NA_real_
    if (has_m && dist_ok) {
      lattice <- .dsomopReleaseLattice(sub$v[in_s], owner$bounds)
      s_int <- lattice$s
      q_int <- lattice$q
      s_noisy <- noisy("S", paste0("stratum=", s), sum(s_int), alloc_s,
                       cap * J, tok, s_int)
      q_noisy <- noisy("Q", paste0("stratum=", s), sum(q_int), alloc_q,
                       cap * J, tok, q_int)
    }
    rows[[i]] <- data.frame(stratum = s, n_noisy = n_noisy,
                            s_noisy = s_noisy, q_noisy = q_noisy,
                            dist_ok = dist_ok, stringsAsFactors = FALSE)
    bins[[i]] <- h_noisy
  }
  strata <- if (length(rows)) do.call(rbind, rows) else
    data.frame(stratum = character(0), n_noisy = numeric(0),
               s_noisy = numeric(0), q_noisy = numeric(0),
               dist_ok = logical(0), stringsAsFactors = FALSE)

  # Displayed values: top-down projection of the signed components.
  if (data_kind) {
    strata$n_signed <- strata$n_noisy
    strata$n_display <- pmax(0, strata$n_noisy)
  } else if (public_kind) {
    strata$n_signed <- strata$n_noisy +
      (root_noisy - sum(strata$n_noisy)) / nrow(strata)
    strata$n_display <- .dsomopReleaseSimplex(strata$n_noisy,
                                              max(0, root_noisy))
  } else {
    strata$n_signed <- strata$n_noisy
    strata$n_display <- max(0, root_noisy)
  }
  display_bins <- lapply(seq_along(bins), function(i) {
    if (!has_h || all(is.na(bins[[i]]))) return(rep(NA_real_, length(bin_keys)))
    .dsomopReleaseSimplex(bins[[i]], strata$n_display[[i]])
  })
  strata$mean <- NA_real_
  strata$sd <- NA_real_
  usable <- strata$n_display >= max(2, support$dist)
  if (has_m) {
    L <- owner$bounds[[1L]]; U <- owner$bounds[[2L]]
    half <- (U - L) / 2; mid <- (U + L) / 2
    for (i in which(usable & strata$dist_ok)) {
      N <- strata$n_display[[i]]
      p <- .dsomopReleaseProjectMoments(strata$s_noisy[[i]] / J,
                                        strata$q_noisy[[i]] / J, N)
      strata$mean[[i]] <- mid + half * p[["S"]] / N
      strata$sd[[i]] <- sqrt(max(0, (p[["Q"]] - p[["S"]]^2 / N) / (N - 1))) *
        half
    }
  }
  quantiles <- lapply(seq_along(display_bins), function(i) {
    if (!has_h || is_cell || !usable[[i]] || !strata$dist_ok[[i]]) {
      return(rep(NA_real_, 5L))
    }
    .dsomopReleaseQuantiles(display_bins[[i]], owner$grid)
  })
  strata$p10 <- vapply(quantiles, `[[`, numeric(1L), 1L)
  strata$p25 <- vapply(quantiles, `[[`, numeric(1L), 2L)
  strata$median <- vapply(quantiles, `[[`, numeric(1L), 3L)
  strata$p75 <- vapply(quantiles, `[[`, numeric(1L), 4L)
  strata$p90 <- vapply(quantiles, `[[`, numeric(1L), 5L)
  for (col in c("n_display", "mean", "sd", "p10", "p25", "median", "p75",
                "p90")) {
    strata[[col]] <- .dsomopReleaseRound(strata[[col]])
  }

  # Signed components for pooling: histogram cells reconciled to the one
  # canonical signed N of their stratum (itself reconciled to the parent of a
  # public partition), plus N, S and Q; no second, independent total.
  export <- lapply(seq_along(strata_keys), function(i) {
    h <- bins[[i]]
    reconciled <- if (has_h && !all(is.na(h)) && length(h) > 0L) {
      h + (strata$n_signed[[i]] - sum(h)) / length(h)
    } else NULL
    list(stratum = strata_keys[[i]], n = strata$n_signed[[i]],
         s = if (has_m) strata$s_noisy[[i]] else NULL,
         q = if (has_m) strata$q_noisy[[i]] else NULL,
         h = reconciled)
  })
  list(
    owner = owner, protection = protection, strata = strata,
    root_n = root_noisy,
    bins = stats::setNames(display_bins, strata_keys), bin_keys = bin_keys,
    export = list(
      protocol = .DSOMOP_RELEASE_PROTOCOL, owner = owner$id,
      population = owner$population, family = owner$family,
      grid = owner$grid$id %||% NULL,
      breaks = if (!is_cell && has_h) owner$grid$breaks else NULL,
      bounds = owner$bounds, strata_kind = owner$strata$kind,
      root = if (public_kind) list(n = root_noisy) else NULL,
      cell_keys = if (has_h) bin_keys else NULL,
      cells = if (is_cell) owner$cells else NULL,
      lattice = if (has_m) J else NULL, cap = cap,
      allocation = c(list(N = frac$N),
                     if (public_kind) list(N_strata = frac$N),
                     list(H = alloc_h, S = alloc_s, Q = alloc_q)),
      reconciliation = if (public_kind) {
        "strata_N_sum_to_root_N_cells_to_stratum_N"
      } else "histogram_cells_sum_to_stratum_N",
      components = export
    )
  )
}

# --- Routes, labels and standard-route masking ----------------------------------

#' Resolve the protection route of one protected-contract request
#'
#' Depends only on the custodial mode, the DP service state, the contract and
#' whether the request is scoped: never on the data or on earlier questions.
#' \itemize{
#'   \item \code{standard} mode: the established standard output, with the
#'     contract's demonstrated-disclosive fields unavailable.
#'   \item \code{auto} mode: \code{fixed_synopsis} for an unscoped question
#'     over the contract's public basis when the custodian declares an
#'     immutable data generation (\code{dsomop.dp.snapshot_id}); otherwise
#'     \code{realised_sticky}. Without the DP service, the standard output with
#'     the disclosive fields unavailable (\code{standard_fallback}).
#'   \item \code{dp} mode: only \code{fixed_synopsis}; anything else is a
#'     stable unsupported-mechanism refusal.
#' }
#'
#' @param handle CDM handle.
#' @param contract Release contract.
#' @param scoped Logical; the request carries a cohort/table scope.
#' @return Route list (protection, mode, policy, reason).
#' @keywords internal
.omopToolsReleaseRoute <- function(handle, contract, scoped) {
  mode <- .dsomopToolsMode()
  always <- isTRUE(contract$protected_always)
  if (identical(mode, "standard") && !always) {
    return(list(protection = "standard", mode = mode))
  }
  # A contract without a protected mechanism (an effect with no bounded
  # sufficient statistics) always takes the standard route with its masked
  # fields; DP-only mode refuses it like any standard statistic.
  if (is.null(contract$run) && !isTRUE(contract$mixed)) {
    .dsomopRequireStandardStatistics()
    return(list(protection = "standard", mode = mode))
  }
  # A mixed frame's binary rows are standard statistics.
  if (isTRUE(contract$mixed)) .dsomopRequireStandardStatistics()
  ready <- .dsomopToolsDpReady(handle)
  if (!isTRUE(ready$ready)) {
    if (identical(mode, "dp")) {
      stop("The data custodian has set the community tools to DP-only mode, ",
           "but the differential-privacy service is not available on this ",
           "server.", call. = FALSE)
    }
    if (always) {
      return(list(protection = "unavailable", mode = mode,
                  reason = ready$reason))
    }
    return(list(protection = "standard_fallback", mode = mode,
                reason = ready$reason))
  }
  policy <- ready$policy
  fixed <- !isTRUE(scoped) && isTRUE(contract$fixed_basis) &&
    isTRUE(policy$custodial_snapshot)
  if (identical(mode, "dp") && !fixed) {
    stop("This release has no fixed-synopsis protection: DP-only mode ",
         "(dsomop.tools.mode = \"dp\" or dsomop.dp.exclusive = TRUE) accepts ",
         "only unscoped questions over a public basis of a custodian-declared ",
         "data generation (dsomop.dp.snapshot_id). In auto mode the same ",
         "question is answered with heuristic realised-contribution ",
         "protection.", call. = FALSE)
  }
  list(protection = if (fixed) "fixed_synopsis" else "realised_sticky",
       mode = mode, policy = policy)
}

#' Public protection label of a protected release
#'
#' Reports the effective estimand, grids, bounds, contribution cap, the static
#' component allocations and noise scales, and the guarantee scope. A
#' \code{realised_sticky} release reports \code{noise_reference_epsilon} and
#' per-component scales only: \code{mechanism_epsilon} and
#' \code{api_transcript_epsilon} are NA (no joint guarantee is claimed). A
#' \code{fixed_synopsis} release reports the epsilon of its declared bounded
#' vector, \code{guarantee_scope = "fixed_vector_before_exact_support_gate"}.
#' Status fields \code{privacy_call_quota = "none"} and
#' \code{history_based_admission = FALSE}. Never contains seeds, fingerprints,
#' person tokens or exact hidden support.
#'
#' @keywords internal
.omopToolsReleaseLabel <- function(route, built_list, contract = NULL,
                                   snap = NULL, extra = list()) {
  policy <- route$policy
  e <- policy$release_epsilon
  owners <- lapply(built_list, function(b) b$owner)
  components <- lapply(built_list, function(b) {
    o <- b$owner
    # Capped record-weighted moments (tools_release_records.R).
    if (identical(o$family, "RM")) return(.dsomopReleaseRecordLabel(b, e))
    cap <- max(1L, o$cap)
    frac <- .DSOMOP_RELEASE_FRACTIONS
    # A public partition owns its parent total and its strata totals.
    stratified <- !is.null(b$export$root)
    alloc <- c(list(N = frac$N), if (stratified) list(N_strata = frac$N))
    if (identical(o$family, "C6")) {
      alloc <- c(alloc, list(status = frac$C6$status, time = frac$C6$time))
      sens <- lapply(names(alloc), function(f) {
        if (identical(f, "time")) cap * .DSOMOP_RELEASE_TIME_CAP else cap
      })
      names(sens) <- names(alloc)
      sd <- lapply(names(alloc), function(f) {
        .dsomopReleaseNoiseSd(e * alloc[[f]], sens[[f]])
      })
      names(sd) <- names(alloc)
      return(list(owner = o$id, estimand = o$estimand, family = "C6",
                  population = o$population,
                  status_owner = o$status_owner %||% o$id,
                  time_owner = o$time_owner %||% o$id,
                  horizon_days = .DSOMOP_RELEASE_TIME_CAP,
                  contribution_cap = cap, allocation_fraction = alloc,
                  allocation_epsilon = lapply(alloc, function(a) a * e),
                  sensitivity = sens, noise_sd = sd))
    }
    if (o$family %in% c("H", "HM")) {
      alloc$H <- if (identical(o$family, "H")) frac$H$H else frac$HM$H
    }
    if (o$family %in% c("HM", "M") && !is.null(o$bounds)) {
      alloc$S <- if (identical(o$family, "HM")) frac$HM$S else frac$M$S
      alloc$Q <- if (identical(o$family, "HM")) frac$HM$Q else frac$M$Q
    }
    sens <- lapply(names(alloc), function(f) {
      if (f %in% c("S", "Q")) cap * .DSOMOP_RELEASE_LATTICE else cap
    })
    names(sens) <- names(alloc)
    sd <- lapply(names(alloc), function(f) {
      s <- .dsomopReleaseNoiseSd(e * alloc[[f]], sens[[f]])
      if (f %in% c("S", "Q")) s / .DSOMOP_RELEASE_LATTICE else s
    })
    names(sd) <- names(alloc)
    list(owner = o$id, estimand = o$estimand, family = o$family,
         population = o$population,
         grid = o$grid$id %||% NA_character_,
         bounds = o$bounds %||% NA_real_, contribution_cap = cap,
         allocation_fraction = alloc,
         allocation_epsilon = lapply(alloc, function(a) a * e),
         sensitivity = sens, noise_sd = sd)
  })
  fixed <- identical(route$protection, "fixed_synopsis")
  # The union of the owned components: a person total shared by several
  # owners of one population counts once.
  union_keys <- character(0)
  union_eps <- numeric(0)
  for (c in components) {
    for (f in names(c$allocation_epsilon)) {
      key <- if (f %in% c("N", "N_strata")) {
        paste(f, c$population %||% c$owner, sep = "\u001f")
      } else paste(c$owner, f, sep = "\u001f")
      if (!key %in% union_keys) {
        union_keys <- c(union_keys, key)
        union_eps <- c(union_eps, c$allocation_epsilon[[f]])
      }
    }
  }
  vector_epsilon <- sum(union_eps)
  label <- list(
    protection = route$protection,
    mode = route$mode,
    contract = .DSOMOP_RELEASE_CONTRACT_VERSION,
    release_protocol = .DSOMOP_RELEASE_PROTOCOL,
    class = contract$class %||% NA_character_,
    guarantee_scope = unname(.DSOMOP_RELEASE_GUARANTEE[route$protection]),
    noise_reference_epsilon = e,
    mechanism_epsilon = if (fixed) vector_epsilon else NA_real_,
    api_transcript_epsilon = NA_real_,
    adjacency = "add_remove_person",
    mechanism = policy$mechanism,
    sampler = policy$sampler,
    sticky = TRUE,
    components = components,
    interval_contract = paste0(
      "bins [b_i, b_i+1) with an open last bin; the first bin is the open ",
      "underflow bin on scales without a public lower bound"),
    quantile_method = paste0(
      "inverse of the displayed protected histogram's CDF, linear within ",
      "finite bins; quantiles in an open tail bin report its finite edge"),
    effective_parameters = snap$changes %||% list(),
    privacy_call_quota = "none",
    history_based_admission = FALSE,
    note = if (fixed) paste0(
      "Fixed-synopsis protection: every value is post-processing of one ",
      "protected vector of this data generation (discrete-Laplace noise, ",
      "reference epsilon ", e, " split across its components). Exact ",
      "support gates and other enabled methods are outside this guarantee."
    ) else paste0(
      "Heuristic realised-contribution protection: discrete-Laplace noise ",
      "(reference epsilon ", e, " split across the components) keyed on the ",
      "people each component contains, so equivalent questions return the ",
      "same answer. It is not a joint differential-privacy guarantee."
    )
  )
  # delta = 0 belongs to the declared fixed vector only; a heuristic
  # realised or mixed release states no delta at all (B14).
  if (fixed) label$delta <- 0
  label$source <- list(logical_source_id =
                         .dsomopReleaseLogicalSourceId(policy))
  if (length(owners) == 1L) {
    label$estimand <- owners[[1L]]$estimand
    label$owner <- owners[[1L]]$id
  }
  for (name in names(extra)) label[[name]] <- extra[[name]]
  if (identical(label$protection, "mixed")) {
    # A mixed frame claims no frame-level epsilon or delta; its blocks name
    # their own protection.
    label$delta <- NULL
    label$mechanism_epsilon <- NA_real_
  }
  label
}

#' Standard-route output of a protected contract
#'
#' The contract's demonstrated-disclosive fields are unavailable on the
#' standard route: exact scoped moments and quantiles (differencing two scopes
#' recovers one person's value) and exact flexible-window person-time and
#' effects (window sweeps recover exact counts and event days). Everything else
#' keeps its established standard controls.
#'
#' @param df Released (gated) standard frame.
#' @param contract Release contract.
#' @param route Route list.
#' @param scoped Logical.
#' @return \code{df} with masked fields and the reason recorded in its label.
#' @keywords internal
.omopToolsReleaseMaskStandard <- function(df, contract, route, scoped) {
  fields <- contract$masked_always %||% character(0)
  if (isTRUE(scoped)) fields <- union(fields, contract$masked_scoped %||%
                                        character(0))
  fields <- intersect(fields, names(df))
  if (length(fields) && is.data.frame(df) && nrow(df) > 0L) {
    for (f in fields) df[[f]] <- rep(NA_real_, nrow(df))
  }
  attr(df, "dsomop_release_masked") <- list(
    fields = fields,
    reason = if (identical(route$protection, "standard_fallback"))
      "protected_mechanism_unavailable" else "protected_output_required"
  )
  df
}

#' Finish the label of a standard-route release
#'
#' Masks a protected contract's demonstrated-disclosive fields and records the
#' masked fields, the reason, the effective parameters and the stateless
#' status fields in the public label.
#'
#' @keywords internal
.omopToolsReleaseStandardLabel <- function(df, contract, route, scoped, snap) {
  if (!is.null(contract) && !is.null(route)) {
    df <- .omopToolsReleaseMaskStandard(df, contract, route, scoped)
  }
  label <- attr(df, "dsomop_protection", exact = TRUE) %||%
    list(protection = "standard")
  masked <- attr(df, "dsomop_release_masked", exact = TRUE)
  if (!is.null(masked)) {
    if (length(masked$fields)) {
      label$masked_fields <- masked$fields
      label$masked_reason <- masked$reason
    }
    attr(df, "dsomop_release_masked") <- NULL
  }
  if (length(snap$changes %||% list())) {
    label$effective_parameters <- snap$changes
  }
  label$privacy_call_quota <- "none"
  label$history_based_admission <- FALSE
  attr(df, "dsomop_protection") <- label
  df
}

#' Stable unavailable status of an always-protected view
#'
#' Used when a view that is only ever released from its protected basis (a
#' threshold view of one protected histogram) has no protected mechanism on
#' this server, or in standard mode without the DP service. After the parent
#' population admission only, it returns the contract's fixed one-row shape
#' with every value unavailable: the raw tail support is never re-tested, so
#' the presence of the row reveals nothing about the threshold.
#'
#' @keywords internal
.omopToolsReleaseUnavailable <- function(handle, params, contract, route,
                                         snap) {
  if (!is.function(contract$unavailable)) {
    stop("This protected view has no stable unavailable shape.",
         call. = FALSE)
  }
  df <- contract$unavailable(handle, params)
  attr(df, "dsomop_protection") <- list(
    protection = "unavailable", mode = route$mode,
    contract = contract$name,
    masked_fields = names(df),
    masked_reason = "protected_mechanism_unavailable",
    effective_parameters = snap$changes,
    privacy_call_quota = "none", history_based_admission = FALSE,
    note = paste0("This view is released only from its protected histogram; ",
                  "the DP service is unavailable, so its values are ",
                  "unavailable (the threshold's raw support is not tested)."))
  df
}

#' Pooling contracts of protected releases
#'
#' In auto and DP-only modes an entry with a protected release contract
#' publishes the legacy \code{not_poolable} strategy (so clients older than
#' 2.10.3 display per-server frames and never pool projected or heuristic
#' protected values as if they were exact sufficient statistics) and a
#' separate \code{release_pooling} descriptor: 2.10.3 clients pool the signed,
#' reconciled components of compatible protected releases and derive views
#' once (record-weighted summaries: the signed N, R, S and Q per stratum). The
#' descriptor also carries the entry's established contract
#' (\code{standard_contract}), by which frames of servers whose DP service is
#' unavailable (standard outputs with the disclosive fields unavailable) are
#' pooled with each other, and, for effect estimates, its established
#' \code{effect_estimate} contract: the pooled effect is the inverse-variance
#' meta-analysis of the per-site protected estimates (post-processing).
#' Standard mode keeps the established contract.
#'
#' @param entry Catalog entry.
#' @param contract Its validated legacy pooling contract.
#' @return The entry with \code{meta$pooling_contract} (and possibly
#'   \code{meta$release_pooling}) set.
#' @keywords internal
.omopToolsReleasePoolingContract <- function(entry, contract) {
  release <- tryCatch(.omopToolsReleaseContract(entry), error = function(e) NULL)
  mode <- tryCatch(.dsomopToolsMode(), error = function(e) "auto")
  if (is.null(release) || identical(mode, "standard") ||
      identical(contract$strategy, "not_poolable")) {
    entry$meta$pooling_contract <- contract
    return(entry)
  }
  entry$meta$release_pooling <- c(
    list(version = 1L, strategy = "protected_components",
         protocol = .DSOMOP_RELEASE_PROTOCOL,
         contract = .DSOMOP_RELEASE_CONTRACT_VERSION,
         release_contract = release$name, class = release$class,
         columns = contract$columns, standard_contract = contract),
    if (identical(contract$strategy, "effect_estimate")) {
      list(effect_estimate = contract)
    })
  entry$meta$pooling_contract <- .omopPoolingNotPoolable(paste0(
    "Protected release (", release$class, "): its values are projected ",
    "noisy components, not exact sufficient statistics. dsOMOPClient >= ",
    "2.10.3 pools the signed components of compatible protected releases."))
  entry
}

# --- C6: person-time status components ---------------------------------------

#' Build, noise and display person-time components (incidence, arms)
#'
#' One row per person and stratum: a first-outcome status (event / no event)
#' and the observed time at risk in whole days. Per stratum the components are
#' the shared person total (0.2e, sensitivity \code{cap}), the status pair
#' (0.3e, sensitivity \code{cap}, projected onto the displayed total) and the
#' observed-time sum with each person's time clipped to the public horizon
#' \code{T = 3650} days (0.5e, sensitivity \code{cap * T}). Rates, proportions,
#' rate ratios and the MDRR are post-processing of these components only.
#' Public strata (a complete partition such as gender) are displayed top-down
#' from their total; independent strata (two comparison arms) one by one.
#'
#' @param policy DP policy.
#' @param protection Protected route.
#' @param owner Owner spec (\code{strata}: "root", a public partition, or
#'   list(kind = "independent", keys)).
#' @param sub Data frame: person_id, stratum, event (0/1), time (days).
#' @return List: \code{strata} (displayed persons, events, time, noise SDs),
#'   \code{export} (signed components) and \code{owner}.
#' @keywords internal
.dsomopReleaseBuildC6 <- function(policy, protection, owner, sub) {
  keys <- .dsomopReleaseKeys(policy)
  support <- .dsomopReleaseSupport()
  frac <- .DSOMOP_RELEASE_FRACTIONS
  T_cap <- .DSOMOP_RELEASE_TIME_CAP
  cap <- max(1L, owner$cap)
  if (!is.data.frame(sub) ||
      !all(c("person_id", "stratum", "event", "time") %in% names(sub))) {
    stop("A person-time substrate lacks its columns.", call. = FALSE)
  }
  sub <- sub[!is.na(sub$person_id) & !is.na(sub$stratum), , drop = FALSE]
  sub$person_id <- .dsomopReleasePersonIdText(sub$person_id)
  sub$event <- as.integer(!is.na(sub$event) & as.numeric(sub$event) > 0)
  sub$time <- pmin(T_cap, pmax(0, floor(suppressWarnings(as.numeric(sub$time)))))
  sub$time[is.na(sub$time)] <- 0
  key <- paste(sub$person_id, sub$stratum, sep = "\u001f")
  if (anyDuplicated(key)) {
    stop("A person-time substrate lists one person twice in one stratum.",
         call. = FALSE)
  }
  if (!identical(owner$strata$kind, "independent") &&
      anyDuplicated(sub$person_id)) {
    stop("A person-time partition lists one person in two strata.",
         call. = FALSE)
  }
  strata_keys <- owner$strata$keys
  if (length(setdiff(unique(sub$stratum), strata_keys))) {
    stop("A person-time substrate has a stratum outside its public domain.",
         call. = FALSE)
  }
  tokens <- if (nrow(sub)) .dsomopReleasePersonTokens(keys, policy,
                                                      sub$person_id) else
    character(0)
  # Status and time belong to their own scientific owners (first-outcome
  # status of this outcome definition; observed person-time of this time
  # contract), never to request labels, scope spelling or window text: the
  # realised maps carry the population and the window. Person totals use the
  # one eligible-person owner.
  status_owner <- owner$status_owner %||% owner$id
  time_owner <- owner$time_owner %||% owner$id
  noisy <- function(family, coordinate, value, allocation, sensitivity,
                    map_tokens, contributions, owner_id) {
    fp <- if (identical(protection, "realised_sticky")) {
      .dsomopReleaseFingerprint(keys, owner_id, family, coordinate,
                                map_tokens, contributions)
    } else NULL
    .dsomopReleaseNoisyComponent(policy, keys, protection, list(
      owner = owner_id, population = owner$population, family = family,
      coordinate = coordinate, value = value, allocation = allocation,
      sensitivity = sensitivity, fingerprint = fp))
  }
  public_kind <- identical(owner$strata$kind, "public") &&
    length(strata_keys) > 1L
  # Coordinates carry public partition cells only: an unstratified population
  # and each independent arm is "stratum=root" of its own map, so the same
  # people answer alike whatever the request labels them (B06).
  coord_of <- function(s) if (public_kind) paste0("stratum=", s) else
    "stratum=root"
  # The canonical parent person total of a public partition, drawn once and
  # never moved by a child.
  root_noisy <- if (public_kind) {
    noisy("N", "stratum=root", nrow(sub), frac$N, cap, tokens,
          rep(1, nrow(sub)), owner_id = .DSOMOP_RELEASE_N_OWNER)
  } else NA_real_
  rows <- lapply(strata_keys, function(s) {
    in_s <- sub$stratum == s
    tok <- tokens[in_s]
    ev <- sub$event[in_s]
    tm <- sub$time[in_s]
    n <- noisy("N", coord_of(s), sum(in_s), frac$N, cap, tok,
               rep(1, sum(in_s)), owner_id = .DSOMOP_RELEASE_N_OWNER)
    e1 <- noisy("status", paste0(coord_of(s), "|status=event"), sum(ev),
                frac$C6$status, cap, tok[ev == 1L], rep(1, sum(ev)),
                owner_id = status_owner)
    e0 <- noisy("status", paste0(coord_of(s), "|status=no_event"),
                sum(1L - ev), frac$C6$status, cap, tok[ev == 0L],
                rep(1, sum(1L - ev)), owner_id = status_owner)
    t <- noisy("T", coord_of(s), sum(tm), frac$C6$time,
               cap * T_cap, tok, tm, owner_id = time_owner)
    data.frame(stratum = s, n_noisy = n, event_noisy = e1,
               no_event_noisy = e0, time_noisy = t,
               n_exact_supported = sum(in_s) >= support$floor,
               stringsAsFactors = FALSE)
  })
  st <- do.call(rbind, rows)
  if (public_kind) {
    # Signed children reconciled to the fixed signed parent (pooling);
    # displayed children projected onto the parent's displayed total.
    st$n_signed <- st$n_noisy + (root_noisy - sum(st$n_noisy)) / nrow(st)
    st$n_display <- .dsomopReleaseSimplex(st$n_noisy, max(0, root_noisy))
  } else {
    st$n_signed <- st$n_noisy
    st$n_display <- pmax(0, st$n_noisy)
  }
  st$events_display <- vapply(seq_len(nrow(st)), function(i) {
    .dsomopReleaseSimplex(c(st$event_noisy[[i]], st$no_event_noisy[[i]]),
                          st$n_display[[i]])[[1L]]
  }, numeric(1L))
  st$time_display <- pmin(pmax(st$time_noisy, 0), st$n_display * T_cap)
  e <- policy$release_epsilon
  sd <- list(
    N = .dsomopReleaseNoiseSd(e * frac$N, cap),
    status = .dsomopReleaseNoiseSd(e * frac$C6$status, cap),
    time = .dsomopReleaseNoiseSd(e * frac$C6$time, cap * T_cap))
  for (col in c("n_display", "events_display", "time_display")) {
    st[[col]] <- .dsomopReleaseRound(st[[col]])
  }
  # Status cells are reconciled to their stratum's signed total before export.
  export <- lapply(seq_len(nrow(st)), function(i) {
    h <- c(st$event_noisy[[i]], st$no_event_noisy[[i]])
    list(stratum = st$stratum[[i]], n = st$n_signed[[i]],
         status = h + (st$n_signed[[i]] - sum(h)) / 2,
         time = st$time_noisy[[i]])
  })
  list(owner = c(owner, list(family = "C6")), protection = protection,
       strata = st, noise_sd = sd, root_n = root_noisy,
       export = list(protocol = .DSOMOP_RELEASE_PROTOCOL, owner = owner$id,
                     status_owner = status_owner, time_owner = time_owner,
                     population = owner$population, family = "C6",
                     strata_kind = owner$strata$kind,
                     root = if (public_kind) list(n = root_noisy) else NULL,
                     horizon_days = T_cap, cap = cap,
                     allocation = c(list(N = frac$N),
                                    if (public_kind) list(N_strata = frac$N),
                                    list(status = frac$C6$status,
                                         time = frac$C6$time)),
                     reconciliation = if (public_kind) {
                       "strata_N_sum_to_root_N_status_cells_to_stratum_N"
                     } else "status_cells_sum_to_stratum_N",
                     components = export))
}

#' Protected rate ratio of two arms and its approximate uncertainty
#'
#' \code{RR = (E_t / T_t) / (E_c / T_c)} from displayed protected components
#' (the Poisson maximum-likelihood rate ratio of a single binary exposure with a
#' log person-time offset). The variance of log RR adds the Poisson sampling
#' term \code{1/E} and the public noise variances of the event and time
#' components (delta method); it carries no repeated-query coverage guarantee
#' under the heuristic route.
#'
#' @param target,comparator One-row displayed strata (events_display,
#'   time_display) of each arm.
#' @param sd Public noise SDs (status, time).
#' @param min_events Minimum displayed events per arm for a ratio.
#' @return Named numeric c(estimate, ci_lo, ci_hi, log_estimate,
#'   se_log_estimate).
#' @keywords internal
.dsomopReleaseRateRatio <- function(target, comparator, sd, min_events) {
  na <- c(estimate = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
          log_estimate = NA_real_, se_log_estimate = NA_real_)
  et <- target$events_display; tt <- target$time_display
  ec <- comparator$events_display; tc <- comparator$time_display
  if (!all(is.finite(c(et, tt, ec, tc))) || et < min_events ||
      ec < min_events || tt <= 0 || tc <= 0) {
    return(na)
  }
  log_rr <- log((et / tt) / (ec / tc))
  var <- 1 / et + sd$status^2 / et^2 + sd$time^2 / tt^2 +
    1 / ec + sd$status^2 / ec^2 + sd$time^2 / tc^2
  se <- sqrt(var)
  out <- c(estimate = exp(log_rr), ci_lo = exp(log_rr - 1.96 * se),
           ci_hi = exp(log_rr + 1.96 * se), log_estimate = log_rr,
           se_log_estimate = se)
  stats::setNames(.dsomopReleaseRound(out), names(out))
}
