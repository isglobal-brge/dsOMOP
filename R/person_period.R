# Regular episode-by-period panels

#' Extract a complete episode-by-period panel
#'
#' Builds on the temporal-covariate contract, adding the episode/time bins that
#' intersect the unique OMOP observation period covering each index date.
#' \code{startDay}/\code{endDay} retain the requested bin boundaries, while
#' \code{observationStartDay}/\code{observationEndDay} delimit the observed part
#' of that bin and \code{daysObserved} is inclusive. Covariate rows remain
#' sparse: absence of a \code{(rowId, timeId, covariateId)} row means zero only
#' when the corresponding \code{personPeriods} row exists. Absolute index/event
#' dates and source row identifiers are never returned. This descriptive panel
#' does not infer a risk set from cohort end or death; use survival or
#' counting-process outputs for explicit time-at-risk analyses.
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

  observation_roster <- .loadTemporalObservationRoster(
    handle, cohort_table
  )
  temporal <- .extractTemporalCovariates(
    handle = handle,
    cohort_table = cohort_table,
    table = table,
    concept_filter = concept_filter,
    bin_width = bin_width,
    window_start = window_start,
    window_end = window_end,
    analyses = analyses,
    filters = filters,
    observation_roster = observation_roster
  )

  time_ref <- temporal$timeRef
  observed_bins <- lapply(seq_len(nrow(observation_roster)), function(i) {
    keep <-
      time_ref$endDay >= observation_roster$observation_start_day[[i]] &
      time_ref$startDay <= observation_roster$observation_end_day[[i]]
    which(keep)
  })
  n_periods <- sum(as.double(lengths(observed_bins)))
  max_rows <- .extractionCap("dsomop.max_memory_rows", 1000000L)
  if (!is.finite(n_periods) || n_periods > max_rows) {
    stop("person_period roster would create ", n_periods,
         " episode-bin rows, exceeding the server in-memory row cap of ",
         max_rows, ".", call. = FALSE)
  }

  rows <- lapply(seq_along(observed_bins), function(i) {
    bins <- observed_bins[[i]]
    if (length(bins) == 0L) return(NULL)
    start_day <- as.integer(time_ref$startDay[bins])
    end_day <- as.integer(time_ref$endDay[bins])
    observation_start <- pmax(
      start_day, observation_roster$observation_start_day[[i]]
    )
    observation_end <- pmin(
      end_day, observation_roster$observation_end_day[[i]]
    )
    data.frame(
      rowId = rep.int(as.integer(observation_roster$cohort_row_id[[i]]),
                      length(bins)),
      timeId = as.integer(time_ref$timeId[bins]),
      startDay = start_day,
      endDay = end_day,
      observationStartDay = as.integer(observation_start),
      observationEndDay = as.integer(observation_end),
      daysObserved = as.integer(observation_end - observation_start + 1L),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  person_periods <- if (length(rows) == 0L) {
    data.frame(
      rowId = integer(0), timeId = integer(0), startDay = integer(0),
      endDay = integer(0), observationStartDay = integer(0),
      observationEndDay = integer(0), daysObserved = integer(0),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, rows)
  }

  list(
    personPeriods = person_periods,
    temporalCovariates = temporal$temporalCovariates,
    covariateRef = temporal$covariateRef,
    timeRef = time_ref,
    personRef = temporal$personRef
  )
}
