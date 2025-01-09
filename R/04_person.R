#' Extract Unique Person IDs from OMOP CDM Table
#'
#' @title Get Unique Person IDs from Table
#' @description
#' Extracts all unique person identifiers from a specified OMOP CDM table. This function performs
#' validation checks on the input table structure and returns a deduplicated vector of person IDs.
#'
#' @details
#' The function performs the following operations:
#' 1. Validates the presence of a required 'person_id' column in the input table
#' 2. Extracts all person IDs from the validated table
#' 3. Deduplicates the IDs to ensure uniqueness
#'
#' This function is particularly useful for:
#' * Patient cohort identification
#' * Data filtering operations
#' * Ensuring unique patient counts
#'
#' @param table A data frame or tibble containing OMOP CDM data. Must include a 'person_id' column.
#'
#' @return A vector containing unique person IDs extracted from the input table.
#'
#' @examples
#' \dontrun{
#' # Example with a valid table
#' patient_data <- data.frame(
#'   person_id = c(1, 1, 2, 3, 3),
#'   value = c(10, 20, 30, 40, 50)
#' )
#' unique_ids <- getPersonIds(patient_data)  # Returns c(1, 2, 3)
#'
#' # Example that would raise an error
#' invalid_data <- data.frame(
#'   subject_id = c(1, 2, 3),
#'   value = c(10, 20, 30)
#' )
#' # getPersonIds(invalid_data)  # Would throw error: "Invalid person filter!"
#' }
#'
getPersonIds <- function(table) {
  # Step 1: Validate table structure
  if (!"person_id" %in% names(table)) {
    stop("Invalid person filter! Table must contain a 'person_id' column.")
  }

  # Step 2: Extract and deduplicate person IDs
  personIds <- unique(table$person_id)
  
  return(personIds)
}

#' Retrieve DataSHIELD's Privacy-Preserving Subset Filter Value
#'
#' @title Get DataSHIELD Subset Filter Threshold
#' @description
#' Retrieves and determines the appropriate subset filter threshold from DataSHIELD's disclosure control
#' settings. This value represents the minimum allowable number of patients in any data subset to
#' maintain privacy and prevent potential re-identification.
#'
#' @details
#' The function follows a hierarchical approach to determine the appropriate filter value:
#' 1. Checks for 'nfilter.subset' setting (primary)
#' 2. Falls back to 'datashield.nfilter.subset' if primary not found
#' 3. Uses 'default.nfilter.subset' as final fallback
#' 4. Returns 0 if no valid setting is found
#'
#' This threshold is crucial for:
#' * Maintaining patient privacy
#' * Preventing disclosure of sensitive information
#' * Ensuring compliance with data protection regulations
#' * Supporting safe analysis of OMOP CDM data
#'
#' @return A numeric value representing the minimum required number of patients for subset operations.
#'         Returns 0 if no filter setting is found.
#'
#' @examples
#' \dontrun{
#' # Get the current subset filter threshold
#' min_patients <- getSubsetFilter()
#'
#' # Example usage in data filtering
#' if (nrow(patient_subset) >= min_patients) {
#'   # Proceed with analysis
#' } else {
#'   # Handle insufficient data case
#' }
#' }
#'
getSubsetFilter <- function() {
  # Step 1: Retrieve all disclosure settings
  nFilter <- dsBase::listDisclosureSettingsDS()
  subsetFilter <- 0
  
  # Step 2: Hierarchical check for filter value
  if (!is.null(nFilter$nfilter.subset)) {
    subsetFilter <- nFilter$nfilter.subset
  } else if (!is.null(nFilter$datashield.nfilter.subset)) {
    subsetFilter <- nFilter$datashield.nfilter.subset
  } else if (!is.null(nFilter$default.nfilter.subset)) {
    subsetFilter <- nFilter$default.nfilter.subset
  }
  
  return(subsetFilter)
}
