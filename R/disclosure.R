# Module: Disclosure Control
# Statistical disclosure control for DataSHIELD compliance.
#
# All thresholds and permissions are read from server-side R options,
# following the DataSHIELD convention of double-fallback:
#   getOption("dsomop.X", getOption("default.dsomop.X", hardcoded_default))
#
# Server admins configure these via Opal admin panel, Armadillo config,
# or Rprofile.site on the DataSHIELD server. Analysts cannot override them.

#' Read all disclosure settings from DataSHIELD server options
#'
#' Returns a named list of all disclosure thresholds and server-gated
#' permissions. Every setting follows the standard DataSHIELD option chain:
#' direct option -> \code{default.*} prefix -> hardcoded fallback.
#'
#' @section Standard DataSHIELD Thresholds:
#' \describe{
#'   \item{\code{nfilter_tab} (default 3)}{Minimum cell count for tabular
#'     outputs. Rows with any count column below this are dropped entirely
#'     (not replaced with NA) to prevent suppression-pattern inference.}
#'   \item{\code{nfilter_subset} (default 3)}{Minimum number of distinct
#'     persons required before any operation proceeds. Prevents cohort
#'     fingerprinting by blocking queries on very small populations.}
#'   \item{\code{nfilter_levels_max} (default 40)}{Maximum distinct levels
#'     allowed in a categorical variable. Prevents exhaustive enumeration
#'     of rare attribute values.}
#'   \item{\code{nfilter_levels_density} (default 0.33)}{Maximum ratio of
#'     distinct levels to total observations. Blocks high-cardinality
#'     attributes that could enable attribute-inference attacks.}
#'   \item{\code{nfilter_string} (default 80)}{Maximum string length for
#'     user-supplied parameters. Limits SQL injection surface.}
#'   \item{\code{nfilter_stringShort} (default 20)}{Short string limit for
#'     identifiers and labels.}
#'   \item{\code{nfilter_noise} (default 0.25)}{DataSHIELD minimum Gaussian
#'     plot-noise variance fraction used by selected plotting methods. dsOMOP
#'     reports this server-owned value, but does not reinterpret it as
#'     epsilon/delta, a generic DP budget, replay-stable noise, or permission to
#'     add noise to arbitrary query results.}
#'   \item{\code{formal_dp_enabled}, \code{sticky_noise_enabled},
#'     \code{privacy_ledger_enabled}}{Explicit capability flags. They are
#'     enabled only after the dedicated DP service has completed its early
#'     bootstrap. Ordinary suppression/banding and the \code{nfilter.noise}
#'     plot-noise variance floor never activate these flags.}
#' }
#'
#' @section dsOMOP-Specific Settings:
#' \describe{
#'   \item{\code{query_strict} (default TRUE)}{When TRUE, only pre-approved
#'     queries on the allowlist can execute. When FALSE, queries are
#'     classified on-the-fly (less safe, for development only). Server
#'     option only - cannot be overridden by client.}
#'   \item{\code{nfilter_dist} (default 10)}{Minimum sample size for safe
#'     percentile/quantile estimation. With fewer values, even clamped
#'     percentiles (p05/p95) can approximate min/max, leaking extreme
#'     individual values.}
#'   \item{\code{nfilter_band} (default 5)}{Band width for count banding. Every
#'     person/record count returned to the client is floored to a multiple of
#'     this width (after the small-cell suppression gate) so that an exact
#'     supra-threshold count is never released. This reduces one-person
#'     differencing resolution (e.g. a 49 -> 47 funnel delta reports 45 for
#'     both), but crossing a band boundary can still change the release;
#'     banding is not a formal privacy accountant. See
#'     \code{\link{.bandCount}}.}
#'   \item{\code{nfilter_age_range} (default 5)}{Minimum inclusive width, in
#'     years, accepted for age-range and closed age-group filters while
#'     disclosure filtering is enabled. Controlled by
#'     \code{dsomop.nfilter.age_range}.}
#'   \item{\code{nfilter_date_range} (default 30)}{Minimum inclusive width, in
#'     calendar days, accepted for bounded date filters while disclosure
#'     filtering is enabled. Controlled by
#'     \code{dsomop.nfilter.date_range}.}
#'   \item{\code{max_feature_specs} (default 1000),
#'     \code{max_pivot_concepts} (default 1000),
#'     \code{max_output_columns} (default 5000),
#'     \code{max_temporal_bins} (default 10000),
#'     \code{max_filter_depth} (default 32),
#'     \code{max_filter_nodes} (default 1024),
#'     \code{max_filter_values} (default 10000), and
#'     \code{max_plan_outputs} (default 100)}{Controller-owned operational
#'     limits for in-memory longitudinal reshaping. They prevent a row-bounded
#'     request from causing an unbounded feature, wide, sparse, temporal, filter
#'     tree, or multi-output expansion. In a federation the client negotiates
#'     the smallest limit advertised by every server.}
#'   \item{\code{max_analysis_scope_tables} (default 8)}{Maximum number of
#'     server-resolved workspace \code{omop.table} objects accepted by one
#'     unified analysis request. Each catalog entry may declare a smaller
#'     \code{scope$max_tables} limit. The total mix of cohort references and
#'     tables is additionally capped at this value plus one.}
#'   \item{\code{max_temp_tables_per_handle} (default 256)}{Maximum number of
#'     session-scoped database objects one OMOP handle may own concurrently.}
#'   \item{\code{age_breaks}}{Public, data-independent lower bounds for age
#'     groups. The final bound starts the open-ended top group. Defaults to a
#'     regular grid at least as wide as \code{nfilter_age_range}; controllers
#'     can set \code{dsomop.age_breaks}. Client age groups must be exact unions
#'     of this grid.}
#'   \item{\code{harmonization_contract_version}, \code{age_semantics},
#'     \code{date_semantics}, \code{date_granularity},
#'     \code{datetime_timezone}, \code{week_start}}{Versioned metadata used by
#'     dsOMOPClient to negotiate one federated policy and reject incompatible
#'     nodes. Datetime-to-date conversion uses the controller-owned
#'     \code{dsomop.datetime_timezone} option (default \code{"UTC"}); calendar
#'     weeks start on Monday.}
#' }
#'
#' @section Server-Gated Opt-Out Permissions:
#' These default to FALSE (locked). Only the server admin / data controller
#' can enable them. Analysts cannot request bypass directly.
#' \describe{
#'   \item{\code{allow_absolute_dates} (default FALSE)}{When FALSE, any
#'     extraction request with \code{date_handling = "absolute"} is rejected.
#'     Raw dates are quasi-identifiers per OMOP Privacy Guidance.
#'     Set via: \code{options(dsomop.allow_absolute_dates = TRUE)}}
#'   \item{\code{allow_sensitive_cols} (default FALSE)}{When FALSE, any
#'     extraction request with \code{block_sensitive = FALSE} is rejected.
#'     Sensitive columns include source_value fields, free text, provider
#'     identifiers, and geographic data.
#'     Set via: \code{options(dsomop.allow_sensitive_columns = TRUE)}}
#' }
#'
#' @return Named list of disclosure thresholds and permissions
#' @keywords internal
.omopDisclosureSettings <- function() {
  dp_status <- .pkg_state$dp_status
  dp_ready <- is.list(dp_status) && isTRUE(dp_status$ready)
  age_range_min <- as.numeric(
    getOption("dsomop.nfilter.age_range",
      getOption("default.dsomop.nfilter.age_range", 5)))
  # Public, data-independent lower bounds for annual-resolution age groups. The
  # final value is the lower bound of the open-ended top group.  Deriving the
  # default from the age-range floor keeps the two server policies coherent
  # (e.g. a 10-year floor yields 0,10,...,90+).
  default_age_width <- if (length(age_range_min) == 1L &&
      is.finite(age_range_min) && age_range_min >= 1) {
    max(5L, as.integer(age_range_min))
  } else {
    5L
  }
  default_age_top <- ceiling(85 / default_age_width) * default_age_width
  default_age_breaks <- seq(0, default_age_top, by = default_age_width)

  settings <- list(
    # --- Standard DataSHIELD thresholds ---
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
    formal_dp_enabled      = dp_ready && isTRUE(dp_status$formal_dp),
    sticky_noise_enabled   = dp_ready && isTRUE(dp_status$sticky_noise),
    privacy_ledger_enabled = dp_ready && isTRUE(dp_status$durable_ledger),
    # --- dsOMOP-specific settings ---
    query_strict         = as.logical(getOption("dsomop.query_strict",
                                getOption("default.dsomop.query_strict", TRUE))),
    nfilter_dist         = as.numeric(getOption("dsomop.nfilter.dist",
                                getOption("default.dsomop.nfilter.dist", 10))),
    nfilter_band         = as.numeric(getOption("dsomop.nfilter.band",
                                getOption("default.dsomop.nfilter.band", 5))),
    nfilter_age_range    = age_range_min,
    nfilter_date_range   = as.numeric(
                                getOption("dsomop.nfilter.date_range",
                                  getOption("default.dsomop.nfilter.date_range", 30))),
    max_feature_specs    = as.numeric(getOption(
                                "dsomop.max_feature_specs",
                                getOption("default.dsomop.max_feature_specs", 1000L))),
    max_pivot_concepts   = as.numeric(getOption(
                                "dsomop.max_pivot_concepts",
                                getOption("default.dsomop.max_pivot_concepts", 1000L))),
    max_output_columns   = as.numeric(getOption(
                                "dsomop.max_output_columns",
                                getOption("default.dsomop.max_output_columns", 5000L))),
    max_temporal_bins    = as.numeric(getOption(
                                "dsomop.max_temporal_bins",
                                getOption("default.dsomop.max_temporal_bins", 10000L))),
    max_filter_depth     = as.numeric(getOption(
                                "dsomop.max_filter_depth",
                                getOption("default.dsomop.max_filter_depth", 32L))),
    max_filter_nodes     = as.numeric(getOption(
                                "dsomop.max_filter_nodes",
                                getOption("default.dsomop.max_filter_nodes", 1024L))),
    max_filter_values    = as.numeric(getOption(
                                "dsomop.max_filter_values",
                                getOption("default.dsomop.max_filter_values", 10000L))),
    max_plan_outputs     = as.numeric(getOption(
                                "dsomop.max_plan_outputs",
                                getOption("default.dsomop.max_plan_outputs", 100L))),
    max_analysis_scope_tables = as.numeric(getOption(
                                "dsomop.max_analysis_scope_tables",
                                getOption("default.dsomop.max_analysis_scope_tables",
                                          8L))),
    max_temp_tables_per_handle = as.numeric(getOption(
                                "dsomop.max_temp_tables_per_handle",
                                getOption("default.dsomop.max_temp_tables_per_handle",
                                          256L))),
    # --- Federated harmonisation contract ---
    harmonization_contract_version = "dsomop-harmonization-v3",
    age_breaks = as.numeric(getOption("dsomop.age_breaks",
                              getOption("default.dsomop.age_breaks",
                                        default_age_breaks))),
    # Match OHDSI Circe cohort criteria: YEAR(index/event date) minus
    # person.year_of_birth. OMOP month/day are nullable, so this remains an
    # annual-resolution difference rather than a birthday-aware completed age.
    age_semantics = "reference_year_minus_year_of_birth",
    date_semantics = "ISO8601_Gregorian_closed_interval",
    date_granularity = "calendar_day",
    datetime_timezone = as.character(
      getOption("dsomop.datetime_timezone",
        getOption("default.dsomop.datetime_timezone", "UTC"))),
    week_start = "Monday",
    # --- Server-gated opt-out permissions ---
    # These default to FALSE (locked). Server admin must explicitly enable.
    allow_absolute_dates = as.logical(getOption("dsomop.allow_absolute_dates",
                                getOption("default.dsomop.allow_absolute_dates", FALSE))),
    allow_sensitive_cols  = as.logical(getOption("dsomop.allow_sensitive_columns",
                                getOption("default.dsomop.allow_sensitive_columns", FALSE)))
  )

  numeric_scalar <- function(name, lower, upper = Inf, integer = FALSE) {
    value <- settings[[name]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < lower || value > upper ||
        (integer && value != floor(value))) {
      stop("Invalid server disclosure option '", name, "': expected ",
           if (integer) "an integer " else "a finite number ", "in [",
           lower, ", ", upper, "].", call. = FALSE)
    }
  }
  logical_scalar <- function(name) {
    value <- settings[[name]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("Invalid server disclosure option '", name,
           "': expected TRUE or FALSE.", call. = FALSE)
    }
  }

  numeric_scalar("nfilter_tab", 0, integer = TRUE)
  numeric_scalar("nfilter_subset", 0, integer = TRUE)
  numeric_scalar("nfilter_levels_max", 1, integer = TRUE)
  numeric_scalar("nfilter_levels_density", .Machine$double.eps, 1)
  numeric_scalar("nfilter_string", 1, integer = TRUE)
  numeric_scalar("nfilter_stringShort", 1, integer = TRUE)
  numeric_scalar("nfilter_noise", 0)
  numeric_scalar("nfilter_dist", 0, integer = TRUE)
  numeric_scalar("nfilter_band", 1, integer = TRUE)
  numeric_scalar("nfilter_age_range", 1, integer = TRUE)
  numeric_scalar("nfilter_date_range", 1, integer = TRUE)
  numeric_scalar("max_feature_specs", 1, integer = TRUE)
  numeric_scalar("max_pivot_concepts", 1, integer = TRUE)
  numeric_scalar("max_output_columns", 1, integer = TRUE)
  numeric_scalar("max_temporal_bins", 1, integer = TRUE)
  numeric_scalar("max_filter_depth", 1, integer = TRUE)
  numeric_scalar("max_filter_nodes", 1, integer = TRUE)
  numeric_scalar("max_filter_values", 1, integer = TRUE)
  numeric_scalar("max_plan_outputs", 1, integer = TRUE)
  numeric_scalar("max_analysis_scope_tables", 1, integer = TRUE)
  numeric_scalar("max_temp_tables_per_handle", 1, integer = TRUE)
  age_breaks <- settings$age_breaks
  if (!is.numeric(age_breaks) || length(age_breaks) < 2L || anyNA(age_breaks) ||
      any(!is.finite(age_breaks)) || any(age_breaks != floor(age_breaks)) ||
      age_breaks[1L] != 0 || any(diff(age_breaks) <= 0) ||
      any(diff(age_breaks) < settings$nfilter_age_range)) {
    stop("Invalid server disclosure option 'age_breaks': expected strictly ",
         "increasing integer lower bounds starting at 0, with every interval ",
         "at least nfilter_age_range years wide.", call. = FALSE)
  }
  for (name in c("formal_dp_enabled", "sticky_noise_enabled",
                 "privacy_ledger_enabled", "query_strict",
                 "allow_absolute_dates", "allow_sensitive_cols")) {
    logical_scalar(name)
  }
  tz <- settings$datetime_timezone
  if (length(tz) != 1L || is.na(tz) || !nzchar(tz) ||
      !tz %in% unique(c("UTC", "GMT", OlsonNames()))) {
    stop("Invalid server disclosure option 'datetime_timezone': expected one ",
         "IANA timezone name.", call. = FALSE)
  }

  settings
}

#' Validate canonical age-group labels against a server age grid
#'
#' A closed label (for example, `20-29`) is valid only when both its lower
#' bound and the following boundary (`30`) occur in the public grid.  An
#' open-ended label (`80+`) is valid when its lower bound occurs in the grid.
#' This admits coarsenings (unions of adjacent bins) but never a finer or
#' shifted client-authored band.
#'
#' @param groups Character vector of age-group labels.
#' @param age_breaks Integer lower bounds; the final bound starts the top group.
#' @return Logical scalar.
#' @keywords internal
.ageGroupsOnGrid <- function(groups, age_breaks) {
  groups <- unlist(groups, use.names = FALSE)
  if (length(groups) == 0L || anyNA(groups)) return(FALSE)
  age_breaks <- as.integer(age_breaks)
  all(vapply(groups, function(group) {
    group <- trimws(as.character(group))
    if (grepl("^[0-9]+\\+$", group)) {
      lower <- suppressWarnings(as.integer(sub("\\+$", "", group)))
      return(length(lower) == 1L && !is.na(lower) && lower %in% age_breaks)
    }
    if (!grepl("^[0-9]+-[0-9]+$", group)) return(FALSE)
    bounds <- suppressWarnings(
      as.integer(strsplit(group, "-", fixed = TRUE)[[1L]]))
    length(bounds) == 2L && !anyNA(bounds) && bounds[1L] <= bounds[2L] &&
      bounds[1L] %in% age_breaks && (bounds[2L] + 1L) %in% age_breaks
  }, logical(1)))
}

#' Is this object a dsOMOP person-bearing table?
#'
#' Tests for the \code{omop.table} class stamped onto every person-bearing
#' assign output by \code{\link{.pseudonymizeIdentifiers}}. The data-manipulation
#' verbs use this as an admission gate: they refuse to operate on anything that
#' is not a dsOMOP-produced, token-keyed, disclosure-controlled frame, so a
#' client cannot smuggle an arbitrary data.frame into the gated merge/filter/
#' bind path.
#'
#' @param x Any object.
#' @return \code{TRUE} if \code{x} carries the \code{omop.table} class.
#' @keywords internal
.is_omop.table <- function(x) inherits(x, "omop.table")

#' Assert minimum unique persons in a dataset
#'
#' Prevents cohort-fingerprinting attacks by ensuring that any operation
#' involves at least \code{nfilter_subset} distinct persons. Without this
#' guard, an attacker could iteratively narrow filters until the result
#' describes a single individual (e.g., "condition X + age 87 + female"
#' might match exactly one person). The error message is deliberately
#' generic to avoid leaking the actual count.
#'
#' @param handle CDM handle (used with \code{sql} to run the count query through
#'   the transparent-reconnect, fail-closed DB path)
#' @param sql Character; SQL returning count of distinct person_id
#' @param n_persons Numeric; pre-computed count
#' @return TRUE invisibly, or stops with an error
#' @keywords internal
.assertMinPersons <- function(handle = NULL, sql = NULL, n_persons = NULL) {
  settings <- .omopDisclosureSettings()
  threshold <- settings$nfilter_subset

  if (!is.null(sql) && !is.null(handle)) {
    # Route the gate's COUNT(DISTINCT person_id) through the reconnect helper so
    # a renewed connection that dropped the cohort temp table FAILS CLOSED here
    # rather than counting against a vanished table and waving the result past.
    result <- .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, sql))
    n <- as.numeric(result[[1]][1])
  } else if (!is.null(n_persons)) {
    n <- as.numeric(n_persons)
  } else {
    stop("Either (handle + sql) or n_persons must be provided.", call. = FALSE)
  }

  if (is.na(n) || n < threshold) {
    stop(
      "Disclosive: operation blocked - insufficient individuals to meet ",
      "disclosure threshold. No further details available.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Band a person count down to a multiple of \code{band_width}
#'
#' Differencing defence for aggregate counts that survive the small-cell
#' suppression check. Suppressing counts below \code{nfilter_subset} does
#' NOT stop an attacker who can read EXACT supra-threshold counts: by
#' narrowing a filter and watching a funnel count change (e.g. 50 -> 47),
#' they recover the size of the differenced subgroup (3 persons) even though
#' no single count was ever below threshold. Rounding DOWN to a fixed band
#' (default 5) destroys that 1-person resolution: both 50 and 47 report as
#' 45, so the delta is no longer observable. Rounding down (floor) - never to
#' nearest - guarantees the reported value never exceeds the true count, so
#' the band can never imply more persons than actually exist.
#'
#' The band width comes from the \code{nfilter_band} disclosure setting (default
#' 5), so it is server-configurable and introspectable via
#' \code{\link{.omopDisclosureSettings}} / \code{omopDisclosureSettingsDS()}.
#' Banding is idempotent: flooring an already-banded value to the same width is a
#' no-op (e.g. \code{.bandCount(45) == 45}), so a value may be passed through
#' more than once without drift.
#'
#' @param n Numeric; the exact count (may be NA)
#' @param band_width Integer; band granularity (default 5, minimum 1). Callers
#'   normally pass \code{settings$nfilter_band} so a single server option governs
#'   every banded count.
#' @return Banded count (multiple of \code{band_width}), or NA if \code{n} is NA
#' @keywords internal
.bandCount <- function(n, band_width = 5L) {
  band_width <- max(as.integer(band_width), 1L)
  if (is.null(n) || length(n) == 0 || is.na(n)) return(NA_real_)
  floor(as.numeric(n) / band_width) * band_width
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
  # "No hints" policy: rows below threshold are DROPPED entirely (not set to NA).
  # Returning NA would reveal that a suppressed subgroup exists, enabling
  # subtraction attacks (total - visible rows = hidden group size).
  # NA counts are also DROPPED (fail-closed), consistent with .assertMinPersons:
  # at every call site NA arises only from empty groups (0-equivalent), so a
  # missing count must not be allowed to pass through as a visible row.
  safe <- rep(TRUE, nrow(df))
  for (col in count_cols) {
    vals <- df[[col]]
    safe <- safe & (!is.na(vals) & vals >= threshold)
  }
  result <- df[safe, , drop = FALSE]
  rownames(result) <- NULL
  result
}

#' Drop rows already flagged suppressed, and remove the flag column
#'
#' "No hints" at the source: a result table must never be RETURNED carrying a
#' \code{suppressed} marker. A suppressed row reveals that a rare
#' concept/value/bin EXISTS (with too few persons), which is itself disclosive.
#' This removes such rows entirely and drops the now-redundant flag column, so
#' the suppressed marker never leaves the server.
#'
#' @param df Data frame possibly containing a logical \code{suppressed} column
#' @param col Character; the flag column name (default \code{"suppressed"})
#' @return \code{df} with suppressed rows and the flag column removed
#' @keywords internal
.dropSuppressed <- function(df, col = "suppressed") {
  if (is.data.frame(df) && col %in% names(df)) {
    df <- df[!(df[[col]] %in% TRUE), , drop = FALSE]
    df[[col]] <- NULL
    rownames(df) <- NULL
  }
  df
}

#' Check if returning distinct levels is safe
#'
#' Two checks prevent attribute-inference attacks on categorical variables:
#' \enumerate{
#'   \item \strong{Max levels}: if the number of distinct values exceeds
#'     \code{nfilter_levels_max}, listing them all would constitute an
#'     exhaustive enumeration (e.g., returning all 1000 profession codes).
#'   \item \strong{Density}: if \code{n_levels / n_total} exceeds
#'     \code{nfilter_levels_density}, each level maps to very few persons,
#'     enabling cross-referencing with external data for re-identification.
#' }
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

# --- Age safety: binned age groups ---

#' Compute safe age groups from year_of_birth
#'
#' Bins annual-resolution ages (reference year minus \code{year_of_birth}) on
#' the controller-owned public age grid. A caller
#' may request a coarser regular grid only when every requested boundary belongs
#' to that server grid. Locally unsupported bins are replaced by \code{NA};
#' their boundaries are never merged or changed from data, so labels remain
#' harmonizable across servers.
#'
#' @param year_of_birth Integer vector; birth years
#' @param index_year Integer vector; reference years (same length or scalar)
#' @param bin_width Optional integer coarsening width in years. \code{NULL} uses
#'   the server grid; non-\code{NULL} boundaries must be a subset of it.
#' @param age_breaks Optional explicit public lower bounds. They must be a
#'   coarsening (subset) of the controller-owned server grid.
#' @param min_cell Integer; suppress bins with fewer than this many persons
#'   (defaults to \code{nfilter.tab}).
#' @param person_id Optional person key parallel to the age vectors. When rows
#'   represent recurrent cohort episodes, each person contributes at most once
#'   to a bin's disclosure support.
#' @return Character vector of age group labels (e.g. "40-44", "85+")
#' @keywords internal
.computeAgeGroups <- function(year_of_birth, index_year, bin_width = NULL,
                              age_breaks = NULL, min_cell = NULL,
                              person_id = NULL) {
  settings <- .omopDisclosureSettings()
  if (is.null(min_cell)) min_cell <- settings$nfilter_tab
  server_breaks <- as.integer(settings$age_breaks)

  # A caller may ask for a coarser regular grid (Achilles uses deciles), but
  # every requested boundary must belong to the controller-owned grid.  The
  # default uses the grid verbatim.  No data-dependent boundary is ever made.
  breaks <- server_breaks
  if (!is.null(age_breaks) && !is.null(bin_width)) {
    stop("Specify age_breaks or bin_width, not both.", call. = FALSE)
  }
  if (!is.null(age_breaks)) {
    numeric_breaks <- suppressWarnings(as.numeric(
      unlist(age_breaks, use.names = FALSE)
    ))
    integer_breaks <- suppressWarnings(as.integer(numeric_breaks))
    if (length(integer_breaks) < 2L || anyNA(integer_breaks) ||
        any(!is.finite(numeric_breaks)) ||
        any(numeric_breaks != integer_breaks) || integer_breaks[1L] != 0L ||
        any(diff(integer_breaks) <= 0L) ||
        !all(integer_breaks %in% server_breaks)) {
      stop("Requested age_breaks must be a strictly increasing coarsening ",
           "of the server age grid starting at 0.", call. = FALSE)
    }
    breaks <- integer_breaks
  }
  if (!is.null(bin_width)) {
    bin_width <- max(as.integer(bin_width), 5L)
    if (length(bin_width) != 1L || is.na(bin_width)) {
      stop("bin_width must be one integer.", call. = FALSE)
    }
    breaks <- seq(0L, max(server_breaks), by = bin_width)
    if (!all(breaks %in% server_breaks)) {
      stop("Requested age bins are finer than, or not aligned to, the ",
           "server age grid.", call. = FALSE)
    }
  }

  ages <- as.integer(index_year) - as.integer(year_of_birth)
  ages[ages < 0] <- NA_integer_

  # Handle all-NA case
  if (all(is.na(ages))) return(rep(NA_character_, length(ages)))

  # The final public break starts the open-ended top group.
  bin_idx <- findInterval(ages, c(breaks, Inf), rightmost.closed = FALSE)
  bin_idx[is.na(ages)] <- NA_integer_
  if (is.null(person_id)) person_id <- seq_along(ages)
  if (length(person_id) != length(ages)) {
    stop("person_id must have the same length as the age vectors.", call. = FALSE)
  }

  # Create initial labels
  n_bins <- length(breaks)
  bin_labels <- c(
    paste0(breaks[-n_bins], "-", breaks[-1L] - 1L),
    paste0(breaks[n_bins], "+")
  )

  count_persons <- function() {
    vapply(seq_len(n_bins), function(i) {
      length(unique(person_id[!is.na(bin_idx) & bin_idx == i &
                                !is.na(person_id)]))
    }, integer(1))
  }
  bin_counts <- count_persons()
  # Local support may suppress values, but it must never alter the public
  # boundaries: data-dependent tail merging made federated labels incompatible.
  unsafe_bins <- which(bin_counts < min_cell)
  if (length(unsafe_bins) > 0L) {
    bin_idx[!is.na(bin_idx) & bin_idx %in% unsafe_bins] <- NA_integer_
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

# --- Filter safety policy ---
#
# Filters narrow the population. An overly specific filter can isolate a
# single individual (targeted probing). The classification below prevents
# this by limiting filter granularity:
#   - "allowed": broad categories (sex, age_group) - low fingerprinting risk
#   - "constrained": narrower ranges validated for minimum width
#     (server-configured inclusive age/date widths)
#   - "blocked": arbitrary thresholds or custom SQL - too specific

#' Classify a filter operation by safety level
#'
#' Filters are classified based on fingerprinting risk. Narrow filters
#' (e.g., exact value thresholds) can isolate individuals; broad filters
#' (e.g., sex, age group) cannot. Constrained filters are allowed only
#' if their parameters meet minimum-width requirements.
#'
#' @param filter_type Character; filter type from the DSL
#' @param filter_params List; filter parameters
#' @return Character; \code{"allowed"}, \code{"constrained"}, or \code{"blocked"}
#' @keywords internal
.classifyFilter <- function(filter_type, filter_params = list()) {
  # Always allowed: categorical with known small domains (low fingerprint risk)
  always_allowed <- c("sex", "cohort", "concept_set", "value_bin")

  # Constrained: allowed only after validating minimum range width.
  # `value_threshold` is the legacy classification name retained for reviewed
  # membership/null predicates; public client-authored numeric thresholds are
  # blocked and numeric ranges use authenticated value_bin contracts. age_group is
  # constrained (not always-allowed) so its bands get the same minimum as
  # age_range -- otherwise groups like c("87-87") would target a single birth
  # year and evade the age_range anti-fingerprinting width gate.
  constrained <- c("age_range", "age_group", "has_concept", "date_range",
                    "min_count",
                    "not_has_concept", "concept_count", "prior_observation",
                    "followup", "visit_count", "has_measurement",
                    "missing_measurement", "value_threshold")

  # Blocked: arbitrary SQL could fingerprint individuals
  blocked <- c("custom")

  if (filter_type %in% always_allowed) return("allowed")
  if (filter_type %in% blocked) return("blocked")
  if (filter_type %in% constrained) {
    # The minimum range-width gates (age, date) are anti-fingerprinting defenses.
    # They scale with the disclosure threshold: when the per-person gate is fully
    # disabled (nfilter_subset == 0, e.g. a trusted/reach context the data
    # controller has opted into) the width minimums relax to 0, so degenerate or
    # deliberately-empty ranges can run. Under any positive threshold the
    # configured minimums (defaults: age 5y, date 30d) apply.
    settings <- .omopDisclosureSettings()
    gate_on <- settings$nfilter_subset > 0
    if (filter_type == "age_range" && gate_on) {
      # Ages are integer years and both bounds are inclusive: 20--24 is the
      # standard five-year epidemiological band (five possible ages).
      range_width <- (filter_params$max %||% 150) -
        (filter_params$min %||% 0) + 1
      if (range_width < settings$nfilter_age_range) return("blocked")
    }
    if (filter_type == "age_group" && gate_on) {
      groups <- filter_params$groups
      if (!.ageGroupsOnGrid(groups, settings$age_breaks)) return("blocked")
    }
    if (filter_type == "date_range" && gate_on) {
      if (!is.null(filter_params$start) && !is.null(filter_params$end)) {
        inclusive_days <- as.numeric(
          as.Date(filter_params$end) - as.Date(filter_params$start)
        ) + 1
        if (!is.finite(inclusive_days) ||
            inclusive_days < settings$nfilter_date_range) return("blocked")
      }
    }
    if (gate_on && !is.null(filter_params$window)) {
      window <- filter_params$window
      if (!is.list(window)) return("blocked")
      start <- suppressWarnings(as.numeric(window$start))
      end <- suppressWarnings(as.numeric(window$end))
      if (!is.null(window$start) && !is.null(window$end)) {
        if (length(start) != 1L || length(end) != 1L ||
            !is.finite(start) || !is.finite(end) ||
            start != floor(start) || end != floor(end) || start > end ||
            (end - start + 1) < settings$nfilter_date_range) {
          return("blocked")
        }
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
#' Even a "safe" filter type (e.g., age_range) can be dangerous if it
#' narrows the population to fewer than \code{nfilter_subset} persons.
#' This check runs the filtered query's person count and blocks execution
#' if the result is too small. The error message is deliberately vague
#' to prevent the attacker from learning the exact population size.
#'
#' @param handle CDM handle
#' @param post_sql Character; SQL returning COUNT(DISTINCT person_id) after filter
#' @return TRUE invisibly; stops otherwise
#' @keywords internal
.assertFilterSafe <- function(handle, post_sql) {
  settings <- .omopDisclosureSettings()
  threshold <- settings$nfilter_subset

  post_result <- .coerce_integer64(
    .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, post_sql)))
  post_n <- as.numeric(post_result[[1]][1])

  if (is.na(post_n) || post_n < threshold) {
    stop("Disclosive: filter would reduce population below disclosure threshold.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Emit a governance audit record for a data-access operation
#'
#' Writes a single structured line to the server log via \code{message()}
#' (stderr). The record is NEVER returned to the analyst - it is visible only
#' to the data controller in the server logs. Its purpose is to let the
#' controller review the \emph{sequence} of extraction and cohort operations
#' and detect differencing / triangulation attempts that per-query disclosure
#' checks cannot catch on their own. Gated by \code{dsomop.audit_log}
#' (default TRUE).
#'
#' @param method Character; the assign-method name
#' @param detail Optional operation parameters (filter / cohort spec), logged
#'   as compact JSON and truncated to keep log size bounded
#' @return invisible(NULL)
#' @keywords internal
.omopAuditLog <- function(method, detail = NULL) {
  enabled <- isTRUE(getOption("dsomop.audit_log",
                              getOption("default.dsomop.audit_log", TRUE)))
  if (!enabled) return(invisible(NULL))

  detail_str <- tryCatch(
    as.character(jsonlite::toJSON(detail, auto_unbox = TRUE, null = "null")),
    error = function(e) "<unserializable>"
  )
  if (length(detail_str) != 1L || is.na(detail_str)) detail_str <- "null"
  if (nchar(detail_str) > 2000L) {
    detail_str <- paste0(substr(detail_str, 1L, 2000L), "...<truncated>")
  }

  message(sprintf("[dsomop-audit] %s method=%s detail=%s",
                  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                  method, detail_str))
  invisible(NULL)
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
