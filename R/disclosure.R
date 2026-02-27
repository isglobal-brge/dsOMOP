# ==============================================================================
# dsOMOP v2 - Disclosure Control Module
# ==============================================================================
# Reads DataSHIELD server options and enforces privacy thresholds.
# Self-contained: does NOT rely on dsBase for checks.
# ==============================================================================

#' Read all disclosure settings from DataSHIELD server options
#'
#' @return Named list of disclosure thresholds and privacy level
#' @keywords internal
.omopDisclosureSettings <- function() {
  list(
    nfilter_tab            = as.numeric(getOption("nfilter.tab",
                                getOption("default.nfilter.tab", 3))),
    nfilter_subset         = as.numeric(getOption("nfilter.subset",
                                getOption("default.nfilter.subset", 3))),
    nfilter_levels_max     = as.numeric(getOption("nfilter.levels.max",
                                getOption("default.nfilter.levels.max", 40))),
    nfilter_levels_density = as.numeric(getOption("nfilter.levels.density",
                                getOption("default.nfilter.levels.density", 0.33))),
    nfilter_string         = as.numeric(getOption("nfilter.string",
                                getOption("default.nfilter.string", 80))),
    nfilter_stringShort    = as.numeric(getOption("nfilter.stringShort",
                                getOption("default.nfilter.stringShort", 20))),
    nfilter_noise          = as.numeric(getOption("nfilter.noise",
                                getOption("default.nfilter.noise", 0.25))),
    catalog_strict         = as.logical(getOption("dsomop.catalog_strict",
                                getOption("default.dsomop.catalog_strict", TRUE)))
  )
}

#' Assert minimum unique persons in a dataset
#'
#' @param conn DBI connection
#' @param sql Character; SQL returning count of distinct person_id
#' @param n_persons Numeric; pre-computed count
#' @return TRUE invisibly, or stops with an error
#' @keywords internal
.assertMinPersons <- function(conn = NULL, sql = NULL, n_persons = NULL) {
  settings <- .omopDisclosureSettings()
  threshold <- settings$nfilter_subset

  if (!is.null(sql) && !is.null(conn)) {
    result <- DBI::dbGetQuery(conn, sql)
    n <- as.numeric(result[[1]][1])
  } else if (!is.null(n_persons)) {
    n <- as.numeric(n_persons)
  } else {
    stop("Either (conn + sql) or n_persons must be provided.", call. = FALSE)
  }

  if (is.na(n) || n < threshold) {
    stop(
      "Disclosive: operation blocked — insufficient individuals to meet ",
      "disclosure threshold. No further details available.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Suppress small cell counts by dropping rows
#'
#' Rows with any count column below the disclosure threshold are removed
#' entirely. This prevents leaking suppression patterns that could be used
#' to reverse-engineer individual-level data.
#'
#' @param df Data frame with one or more count columns
#' @param count_cols Character vector; names of count columns to check
#' @return Data frame with disclosive rows removed
#' @keywords internal
.suppressSmallCounts <- function(df, count_cols = "n") {
  if (nrow(df) == 0) return(df)
  settings <- .omopDisclosureSettings()
  threshold <- settings$nfilter_tab
  count_cols <- intersect(count_cols, names(df))
  if (length(count_cols) == 0) return(df)
  safe <- rep(TRUE, nrow(df))
  for (col in count_cols) {
    vals <- df[[col]]
    safe <- safe & (is.na(vals) | vals >= threshold)
  }
  result <- df[safe, , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Check if returning distinct levels is safe
#'
#' @param n_levels Integer; number of distinct levels
#' @param n_total Integer; total number of non-NA values
#' @return TRUE invisibly, or stops with an error
#' @keywords internal
.assertSafeLevels <- function(n_levels, n_total) {
  settings <- .omopDisclosureSettings()

  if (n_levels > settings$nfilter_levels_max) {
    stop(
      "Disclosive: number of distinct levels (", n_levels,
      ") exceeds nfilter.levels.max (", settings$nfilter_levels_max, ").",
      call. = FALSE
    )
  }

  if (n_total > 0) {
    density <- n_levels / n_total
    if (density > settings$nfilter_levels_density) {
      stop(
        "Disclosive: levels density (", round(density, 3),
        ") exceeds nfilter.levels.density (", settings$nfilter_levels_density, ").",
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' Validate a user-supplied string against nfilter.string limits
#'
#' @param s Character string to validate
#' @param short Logical; if TRUE, use nfilter.stringShort limit
#' @return The string (trimmed), or stops if too long
#' @keywords internal
.validateString <- function(s, short = FALSE) {
  if (is.null(s) || length(s) == 0) return(NULL)
  s <- trimws(as.character(s)[[1]])
  settings <- .omopDisclosureSettings()
  limit <- if (short) settings$nfilter_stringShort else settings$nfilter_string

  if (nchar(s) > limit) {
    stop(
      "String too long (", nchar(s), " chars); limit is ", limit, ".",
      call. = FALSE
    )
  }
  s
}

# ==============================================================================
# Age Safety: Binned Age Groups
# ==============================================================================

#' Compute safe age groups from year_of_birth
#'
#' Bins ages into groups with minimum bin width (default 5 years).
#' Merges small-count bins at extremes to prevent identification.
#'
#' @param year_of_birth Integer vector; birth years
#' @param index_year Integer vector; reference years (same length or scalar)
#' @param bin_width Integer; minimum bin width in years (default 5, minimum 5)
#' @param min_cell Integer; merge bins with fewer than this many persons (nfilter.tab)
#' @return Character vector of age group labels (e.g. "40-44", "85+")
#' @keywords internal
.computeAgeGroups <- function(year_of_birth, index_year, bin_width = 5L,
                               min_cell = NULL) {
  bin_width <- max(as.integer(bin_width), 5L)  # Floor at 5
  if (is.null(min_cell)) {
    min_cell <- .omopDisclosureSettings()$nfilter_tab
  }

  ages <- as.integer(index_year) - as.integer(year_of_birth)
  ages[ages < 0] <- NA_integer_

  # Handle all-NA case
  if (all(is.na(ages))) return(rep(NA_character_, length(ages)))

  max_age <- max(ages, na.rm = TRUE)

  # Create initial breaks

  breaks <- seq(0, max_age + bin_width, by = bin_width)
  if (breaks[length(breaks)] <= max_age) {
    breaks <- c(breaks, breaks[length(breaks)] + bin_width)
  }

  # Assign each age to a bin index
  bin_idx <- findInterval(ages, breaks, rightmost.closed = FALSE)
  bin_idx[is.na(ages)] <- NA_integer_

  # Create initial labels
  n_bins <- length(breaks) - 1L
  bin_labels <- character(n_bins)
  for (i in seq_len(n_bins)) {
    bin_labels[i] <- paste0(breaks[i], "-", breaks[i + 1L] - 1L)
  }

  # Count persons per bin
  bin_counts <- tabulate(bin_idx, nbins = n_bins)

  # Merge small-count bins from the top end
  while (n_bins > 1 && bin_counts[n_bins] < min_cell) {
    # Merge last two bins
    bin_counts[n_bins - 1L] <- bin_counts[n_bins - 1L] + bin_counts[n_bins]
    bin_counts <- bin_counts[-n_bins]
    bin_labels[n_bins - 1L] <- paste0(breaks[n_bins - 1L], "+")
    bin_labels <- bin_labels[-n_bins]
    bin_idx[!is.na(bin_idx) & bin_idx == n_bins] <- n_bins - 1L
    n_bins <- n_bins - 1L
  }

  # Merge small-count bins from the bottom end
  while (n_bins > 1 && bin_counts[1] < min_cell) {
    bin_counts[2] <- bin_counts[1] + bin_counts[2]
    bin_counts <- bin_counts[-1]
    lo <- breaks[1]
    # Update label for merged bin
    if (grepl("\\+$", bin_labels[2])) {
      bin_labels[2] <- paste0(lo, "+")
    } else {
      hi_part <- sub("^[0-9]+-", "", bin_labels[2])
      bin_labels[2] <- paste0(lo, "-", hi_part)
    }
    bin_labels <- bin_labels[-1]
    bin_idx[!is.na(bin_idx) & bin_idx == 1L] <- 2L
    bin_idx[!is.na(bin_idx)] <- bin_idx[!is.na(bin_idx)] - 1L
    n_bins <- n_bins - 1L
  }

  # Map bin indices to labels
  result <- rep(NA_character_, length(ages))
  for (i in seq_along(ages)) {
    if (!is.na(bin_idx[i]) && bin_idx[i] >= 1 && bin_idx[i] <= n_bins) {
      result[i] <- bin_labels[bin_idx[i]]
    }
  }
  result
}

# ==============================================================================
# Filter Safety Policy
# ==============================================================================

#' Classify a filter operation by safety level
#'
#' @param filter_type Character; filter type from the DSL
#' @param filter_params List; filter parameters
#' @return Character; "allowed", "constrained", or "blocked"
#' @keywords internal
.classifyFilter <- function(filter_type, filter_params = list()) {
  # Always allowed: categorical with known small domains
  always_allowed <- c("sex", "age_group", "cohort", "concept_set", "value_bin")

  # Constrained: allowed with validation
  constrained <- c("age_range", "has_concept", "date_range", "min_count")

  # Blocked: could fingerprint individuals
  blocked <- c("value_threshold", "custom")

  if (filter_type %in% always_allowed) return("allowed")
  if (filter_type %in% blocked) return("blocked")
  if (filter_type %in% constrained) {
    # Additional validation for constrained filters
    if (filter_type == "age_range") {
      range_width <- (filter_params$max %||% 150) - (filter_params$min %||% 0)
      if (range_width < 5) return("blocked")
    }
    if (filter_type == "date_range") {
      if (!is.null(filter_params$start) && !is.null(filter_params$end)) {
        diff_days <- as.numeric(
          as.Date(filter_params$end) - as.Date(filter_params$start)
        )
        if (diff_days < 30) return("blocked")
      }
    }
    return("constrained")
  }
  "blocked"  # Unknown filters are blocked
}

#' Validate a filter before execution
#'
#' Checks classification and stops with a disclosure error if the filter
#' type is blocked.
#'
#' @param filter_type Character
#' @param filter_params List
#' @return TRUE invisibly if safe; stops otherwise
#' @keywords internal
.validateFilter <- function(filter_type, filter_params = list()) {
  classification <- .classifyFilter(filter_type, filter_params)

  if (classification == "blocked") {
    stop(
      "Disclosive: filter type '", filter_type,
      "' is not allowed (could fingerprint individuals).",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Assert that a filter doesn't reduce population below threshold
#'
#' @param handle CDM handle
#' @param post_sql Character; SQL returning COUNT(DISTINCT person_id) after filter
#' @return TRUE invisibly; stops otherwise
#' @keywords internal
.assertFilterSafe <- function(handle, post_sql) {
  settings <- .omopDisclosureSettings()
  threshold <- settings$nfilter_subset

  post_result <- .coerce_integer64(DBI::dbGetQuery(handle$conn, post_sql))
  post_n <- as.numeric(post_result[[1]][1])

  if (is.na(post_n) || post_n < threshold) {
    stop("Disclosive: filter would reduce population below disclosure threshold.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Validate a table or column identifier (whitelist-based)
#'
#' @param name Character; identifier to validate
#' @param what Character; label for error messages
#' @return The validated name, or stops with an error
#' @keywords internal
.validateIdentifier <- function(name, what = "identifier") {
  if (is.null(name) || length(name) == 0) return(NULL)
  name <- trimws(as.character(name)[[1]])

  if (!grepl("^[A-Za-z_][A-Za-z0-9_.]*$", name)) {
    stop(
      "Invalid ", what, " name '", name,
      "': must start with a letter or underscore, ",
      "and contain only letters, digits, underscores, and dots.",
      call. = FALSE
    )
  }
  name
}
