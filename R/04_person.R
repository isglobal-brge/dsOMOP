#' Get Unique Person IDs from a Table
#'
#' This function extracts all unique person IDs from the specified table. It checks if the table
#' contains a 'person_id' column and returns a vector of unique person IDs. If the 'person_id'
#' column is not present, it stops the execution with an error message.
#'
#' @param table A data frame or tibble containing the data from which to extract person IDs.
#'
#' @return A vector containing all unique person IDs from the table.
#'
getPersonIds <- function(table) {
  # Checks if the table has a 'person_id' column
  if (!"person_id" %in% names(table)) {
    stop("Invalid person filter!")
  }

  personIds <- unique(table$person_id)
  return(personIds)
}


#' Get DataSHIELD's Subset Filter Value
#'
#' This function retrieves the subset filter value (nfilter.subset) from DataSHIELD's disclosure settings.
#' The subset filter value determines the minimum number of patients that can result from a subset operation.
#' It is used as a measure of the minimum number of patients that can be processed, as many tables in OMOP CDM
#' may contain multiple records related to the same patient (person).
#'
#' @return A numeric value representing the minimum number of patients required for subset operations.
#'
getSubsetFilter <- function() {
  nFilter <- dsBase::listDisclosureSettingsDS()
  subsetFilter <- nFilter$nfilter.subset
  return(subsetFilter)
}
