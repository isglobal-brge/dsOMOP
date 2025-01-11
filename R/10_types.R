#' Convert Concept ID Columns to Factors in the Table
#'
#' @title Convert Concept ID Columns to Factors
#' @description
#' Converts all columns in a data frame that contain "concept_id" in their name to factors,
#' ignoring case sensitivity. This is useful for ensuring consistent data types across
#' concept identifier columns in OMOP CDM data.
#'
#' @details
#' The function performs the following operations:
#' 1. Identifies columns containing "concept_id" in their name (case-insensitive)
#' 2. Converts identified columns to factor type using as.factor()
#' 3. Preserves all other columns unchanged
#'
#' Common Use Cases:
#' * Standardizing concept ID columns after data import
#' * Preparing data for statistical analysis
#' * Ensuring consistent data types across concept identifiers
#'
#' @param table A data frame containing one or more columns with "concept_id" in their names.
#'              The columns can be of any type that can be converted to factors.
#'
#' @return A data frame with all concept ID columns converted to factors. All other
#'         columns remain unchanged.
#'
#' @examples
#' \dontrun{
#' # Example with concept ID columns
#' data <- data.frame(
#'   person_id = 1:3,
#'   condition_concept_id = c(1234, 5678, 1234),
#'   drug_concept_id = c(8901, 2345, 6789)
#' )
#' 
#' # Convert concept IDs to factors
#' data_with_factors <- conceptsToFactors(data)
#' }
#'
conceptsToFactors <- function(table) {
  # Step 1: Iterate through all columns in the table
  for (column in names(table)) {
    # Step 2: Check if column name contains "concept_id" (case-insensitive)
    if (grepl("concept_id", column, ignore.case = TRUE)) {
      # Step 3: Convert matching columns to factor type
      table[[column]] <- as.factor(table[[column]])
    }
  }
  
  # Step 4: Return modified table
  return(table)
}

#' Convert Date and DateTime Columns in the Table
#'
#' @title Convert Date and DateTime Columns
#' @description
#' Processes a data frame to convert columns containing date or datetime information
#' to appropriate R date types. The function identifies relevant columns based on
#' their names and converts them to either Date or POSIXct format.
#'
#' @details
#' The function performs these key operations:
#' 1. Column Name Analysis:
#'    * Splits column names at periods
#'    * Examines all parts except the first token
#'    * Identifies date/datetime patterns
#'
#' 2. Type Conversion:
#'    * Converts "_datetime" columns to POSIXct
#'    * Converts "_date" columns to Date
#'    * Preserves other columns unchanged
#'
#' Column Naming Rules:
#' * "_datetime" suffix triggers POSIXct conversion
#' * "_date" suffix triggers Date conversion
#' * First token in period-separated names is ignored
#' * Case-insensitive pattern matching
#'
#' @param table A data frame containing columns that may have "_date" or "_datetime"
#'              in their names. The date/time data should be in a format convertible
#'              by as.Date() or as.POSIXct().
#'
#' @return A data frame with date/datetime columns converted to appropriate R types.
#'         All other columns remain unchanged.
#'
#' @examples
#' \dontrun{
#' # Example with various date columns
#' data <- data.frame(
#'   visit.start_date = c("2023-01-01", "2023-01-02"),
#'   measurement.measurement_datetime = c("2023-01-01 10:30:00", "2023-01-02 14:45:00"),
#'   other_column = c("A", "B")
#' )
#'
#' # Convert date/datetime columns
#' converted_data <- convertDateColumns(data)
#' }
#'
convertDateColumns <- function(table) {
  # Step 1: Process each column in the table
  for (column in names(table)) {
    # Step 2: Split column name into tokens at periods
    tokens <- unlist(strsplit(column, ".", fixed = TRUE))
    
    # Step 3: Process columns with multiple period-separated parts
    if (length(tokens) > 1) {
      # Remove first token and rejoin remaining parts
      tokens <- tokens[-1]  # Exclude first token
      joined_tokens <- paste(tokens, collapse = ".")  # Recombine remaining tokens
      
      # Step 4: Convert to appropriate date/time type based on pattern
      if (grepl("_datetime", joined_tokens, ignore.case = TRUE)) {
        # Convert to POSIXct for datetime columns
        table[[column]] <- as.POSIXct(table[[column]])
      } else if (grepl("_date", joined_tokens, ignore.case = TRUE)) {
        # Convert to Date for date columns
        table[[column]] <- as.Date(table[[column]])
      }
    }
  }
  
  # Step 5: Return processed table
  return(table)
}
