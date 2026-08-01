# Regular episode-by-period panels

#' Extract a complete episode-by-period panel
#'
#' Builds on the temporal-covariate contract, adding the complete Cartesian
#' roster of cohort episodes and relative time bins. Covariate rows remain
#' sparse: absence of a \code{(rowId, timeId, covariateId)} row means zero.
#' Absolute index/event dates and source row identifiers are never returned.
#'
#' @param handle CDM handle.
#' @param cohort_table Cohort table containing episode dates.
#' @param table OMOP event table.
#' @param concept_filter Concept IDs to include.
#' @param bin_width Positive integer bin width in days.
#' @param window_start,window_end Inclusive integer offsets from index.
#' @param analyses Subset of \code{binary} and \code{count}.
#' @param grain Must be \code{"episode"}.
#' @param time_origin Must be \code{"index"}.
#' @param filters Optional reviewed row-filter tree.
#' @return A list containing \code{personPeriods}, sparse
#'   \code{temporalCovariates}, \code{covariateRef}, \code{timeRef}, and the
#'   complete episode-to-person \code{personRef}.
#' @keywords internal
.extractPersonPeriod <- function(handle, cohort_table, table,
                                 concept_filter = NULL,
                                 bin_width = 30L,
                                 window_start = -365L,
                                 window_end = 0L,
                                 analyses = c("binary"),
                                 grain,
                                 time_origin,
                                 filters = NULL) {
  if (!is.character(grain) || length(grain) != 1L || is.na(grain) ||
      !identical(tolower(grain), "episode")) {
    stop("person_period grain must be explicitly 'episode'.", call. = FALSE)
  }
  if (!is.character(time_origin) || length(time_origin) != 1L ||
      is.na(time_origin) || !identical(tolower(time_origin), "index")) {
    stop("person_period time_origin must be explicitly 'index'.",
         call. = FALSE)
  }

  temporal <- .extractTemporalCovariates(
    handle = handle,
    cohort_table = cohort_table,
    table = table,
    concept_filter = concept_filter,
    bin_width = bin_width,
    window_start = window_start,
    window_end = window_end,
    analyses = analyses,
    filters = filters
  )

  n_periods <- as.double(nrow(temporal$personRef)) *
    as.double(nrow(temporal$timeRef))
  max_rows <- .extractionCap("dsomop.max_memory_rows", 1000000L)
  if (!is.finite(n_periods) || n_periods > max_rows) {
    stop("person_period roster would create ", n_periods,
         " episode-bin rows, exceeding the server in-memory row cap of ",
         max_rows, ".", call. = FALSE)
  }

  time_ref <- temporal$timeRef
  row_ids <- temporal$personRef$rowId
  person_periods <- data.frame(
    rowId = rep(as.integer(row_ids), each = nrow(time_ref)),
    timeId = rep(as.integer(time_ref$timeId), times = length(row_ids)),
    startDay = rep(as.integer(time_ref$startDay), times = length(row_ids)),
    endDay = rep(as.integer(time_ref$endDay), times = length(row_ids)),
    stringsAsFactors = FALSE
  )

  list(
    personPeriods = person_periods,
    temporalCovariates = temporal$temporalCovariates,
    covariateRef = temporal$covariateRef,
    timeRef = time_ref,
    personRef = temporal$personRef
  )
}
