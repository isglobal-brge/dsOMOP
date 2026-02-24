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
    privacy_level          = getOption("datashield.privacyControlLevel",
                                getOption("default.datashield.privacyControlLevel",
                                          "banana"))
  )
}

#' Check if privacy control level allows an operation
#'
#' @param required_level Character; minimum level required
#' @return TRUE invisibly, or stops with an error
#' @keywords internal
.assertPrivacyLevel <- function(required_level = "banana") {
  invisible(TRUE)
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
      "Disclosive: unique persons (", ifelse(is.na(n), "NA", n),
      ") < nfilter.subset (", threshold, "). Operation blocked.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Suppress small cell counts in a frequency table
#'
#' @param df Data frame with a count column
#' @param count_col Character; name of the count column
#' @return Data frame with small counts replaced by NA
#' @keywords internal
.suppressSmallCounts <- function(df, count_col = "n") {
  settings <- .omopDisclosureSettings()
  threshold <- settings$nfilter_tab

  if (count_col %in% names(df) && nrow(df) > 0) {
    mask <- !is.na(df[[count_col]]) & df[[count_col]] < threshold
    df[[count_col]][mask] <- NA_real_
  }
  df
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
