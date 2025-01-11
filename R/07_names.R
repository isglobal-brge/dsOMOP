#' Standardize Names for OMOP CDM Data Processing
#'
#' @title Name Standardization for OMOP CDM
#' @description
#' Standardizes names to ensure consistent formatting and compatibility across the OMOP CDM
#' data processing pipeline. Implements a series of transformations to create valid,
#' standardized R object names while maintaining readability and usability.
#'
#' @details
#' The function applies the following standardization steps in sequence:
#' 1. Syntax Validation:
#'    * Converts input to valid R object names using make.names()
#'    * Handles special characters and spaces appropriately
#'
#' 2. Case Standardization:
#'    * Converts all characters to lowercase
#'    * Ensures consistent case formatting across names
#'
#' 3. Delimiter Processing:
#'    * Replaces periods with underscores for better readability
#'    * Consolidates multiple underscores into single ones
#'    * Removes leading and trailing underscores
#'
#' Common Use Cases:
#' * Standardizing concept names from vocabulary
#' * Processing column names in reshaped tables
#' * Preparing names for database operations
#'
#' @param name A character string to be standardized. Can include spaces, special
#'             characters, or mixed case formatting.
#'
#' @return A character string containing the standardized name, formatted according
#'         to the specified rules and suitable for use in R operations.
#'
#' @examples
#' \dontrun{
#' # Basic name standardization
#' standardizeName("Blood Pressure")  # Returns "blood_pressure"
#'
#' # Handling special characters
#' standardizeName("HDL-C (mg/dL)")  # Returns "hdl_c_mg_dl"
#'
#' # Processing multiple delimiters
#' standardizeName("lab.test...result")  # Returns "lab_test_result"
#'
#' # Removing extra underscores
#' standardizeName("__extra_spaces__")  # Returns "extra_spaces"
#' }
#'
standardizeName <- function(name) {
  # Step 1: Convert to valid R object name
  # Handles spaces, special characters, and invalid syntax
  name <- make.names(name)
  
  # Step 2: Standardize case
  # Convert to lowercase for consistency
  name <- tolower(name)
  
  # Step 3: Process delimiters
  # Replace periods with underscores for readability
  name <- gsub("\\.", "_", name)
  
  # Step 4: Clean up underscores
  # Remove duplicate underscores
  name <- gsub("_+", "_", name)
  
  # Step 5: Trim edges
  # Remove leading/trailing underscores
  name <- gsub("^_|_$", "", name)
  
  return(name)
}
