# Assignment policy is separate from aggregate admission. The blueprint's
# sensitive/blocked flags and .computeAgeGroups remain aggregate contracts.

#' Recognise assignment quasi-identifiers
#' @param column One column name.
#' @return Logical scalar.
#' @keywords internal
.assignmentQuasiIdentifier <- function(column) {
  is.character(column) && length(column) == 1L && !is.na(column) &&
    grepl(paste0("(^|_)(dob|date_of_birth|birth_date|birth_datetime)($|_)|",
                 "^(year|month|day)_of_birth$|",
                 "^(age|age_group|birth_year_band)$|_date$|_datetime$"),
          tolower(column))
}

#' Classify a blocked column at an assignment boundary
#' @param column One column name.
#' @param col_df Optional blueprint column metadata for the source table.
#' @return Logical scalar; exact birth components are permitted quasi-identifiers.
#' @keywords internal
.assignmentBlockedColumn <- function(column, col_df = NULL) {
  if (!is.character(column) || length(column) != 1L || is.na(column) ||
      !nzchar(trimws(column))) return(TRUE)
  column <- tolower(trimws(column))
  row <- if (is.data.frame(col_df) && "column_name" %in% names(col_df)) {
    col_df[tolower(col_df$column_name) == column, , drop = FALSE]
  } else NULL
  if (!is.null(row) && "is_untyped_identifier" %in% names(row) &&
      any(row$is_untyped_identifier %in% TRUE)) return(TRUE)
  untyped <- .untypedIdentifierColumns(column,
    reviewed = c(.identifierColumns(), .reviewedIdentifierColumns()),
    allow_concepts = TRUE)
  if (length(untyped)) return(TRUE)
  birth_pattern <- paste0("(^|_)(dob|date_of_birth|birth_date|birth_datetime)($|_)|",
                          "^(year|month|day)_of_birth$")
  if (grepl(birth_pattern, column)) {
    # A birth field does not exempt an independently sensitive source value or
    # narrative name such as birth_date_source_value.
    remaining <- gsub("^(year|month|day)_of_birth$", "birth_component", column)
    remaining <- gsub("dob|date_of_birth|birth_datetime|birth_date",
                      "birth_component", remaining)
    return(.detectSensitiveColumns(remaining))
  }
  if (!is.null(row) && "is_blocked" %in% names(row) &&
      any(is.na(row$is_blocked) | row$is_blocked)) return(TRUE)
  .detectSensitiveColumns(column)
}

#' Band annual ages without aggregate suppression
#' @param ages Annual ages; missing and negative values yield missing bands.
#' @param age_breaks Increasing non-negative integer lower bounds. NULL uses the
#'   controller's default public grid without imposing its aggregate width policy.
#' @return Character vector of age bands, including an open-ended final band.
#' @keywords internal
.bandAges <- function(ages, age_breaks = NULL) {
  if (is.null(age_breaks)) {
    age_breaks <- getOption("dsomop.age_breaks",
      getOption("default.dsomop.age_breaks", seq(0L, 85L, 5L)))
  }
  breaks <- suppressWarnings(as.numeric(unlist(age_breaks, use.names = FALSE)))
  if (length(breaks) < 2L || anyNA(breaks) || any(!is.finite(breaks)) ||
      any(breaks != floor(breaks)) || any(breaks < 0) || any(diff(breaks) <= 0)) {
    stop("age_breaks must be strictly increasing non-negative integer bounds.",
         call. = FALSE)
  }
  ages <- suppressWarnings(as.numeric(ages))
  ages[!is.finite(ages) | ages < 0] <- NA_real_
  labels <- c(paste0(head(breaks, -1L), "-", tail(breaks, -1L) - 1L),
              paste0(tail(breaks, 1L), "+"))
  index <- findInterval(ages, c(breaks, Inf))
  result <- rep(NA_character_, length(ages))
  valid <- !is.na(index) & index >= 1L & index <= length(labels)
  result[valid] <- labels[index[valid]]
  result
}
