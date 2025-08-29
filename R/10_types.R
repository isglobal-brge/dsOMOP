#' Convert Concept ID Columns to Factors in the Table
#'
#' @title Convert Concept ID Columns to Factors
#' @description
#' Converts original concept ID columns in a data frame to factors, while preserving
#' the data types of fallback columns created during concept translation failures.
#' This ensures proper handling of both translated and untranslated concept data.
#'
#' @details
#' The function performs the following operations:
#' 1. Identifies columns ending with "_concept_id" (case-insensitive)
#' 2. Distinguishes between simple numeric fallback patterns and real concept ID columns:
#'    - Simple fallback: "concept_id_123" (numeric data, preserved as-is)
#'    - Real concept ID: "condition_concept_id" or "concept_id_123.value_as_concept_id" 
#' 3. Converts real concept ID columns to factor type using as.factor()
#' 4. Preserves data types of simple numeric fallback columns
#'
#' Common Use Cases:
#' * Standardizing original concept ID columns after data import
#' * Preparing data for statistical analysis while preserving numeric fallback columns
#' * Ensuring consistent data types across concept identifiers without breaking numeric data
#'
#' @param table A data frame containing one or more columns with "concept_id" in their names.
#'              The columns can be of any type that can be converted to factors.
#'
#' @return A data frame with all concept ID columns converted to factors. All other
#'         columns remain unchanged.
#'
#' @examples
#' \dontrun{
#' # Example with various concept ID column types
#' data <- data.frame(
#'   person_id = 1:3,
#'   condition_concept_id = c(1234, 5678, 1234),      # Original -> factor
#'   drug_concept_id = c(8901, 2345, 6789),           # Original -> factor  
#'   concept_id_123 = c(1.5, 2.3, 3.1),               # Simple fallback -> numeric
#'   concept_id_456.value_as_concept_id = c("A", "B", "C")  # Complex fallback -> factor
#' )
#' 
#' data_with_factors <- conceptsToFactors(data)
#' # condition_concept_id: factor
#' # drug_concept_id: factor
#' # concept_id_123: numeric (preserved)
#' # concept_id_456.value_as_concept_id: factor
#' }
#'
conceptsToFactors <- function(table) {
  # Step 1: Iterate through all columns in the table
  for (column in names(table)) {
    # Step 2: Determine if this column should be converted to factor
    # Logic: Convert to factor if column name ends with "_concept_id" 
    # BUT NOT if it's a simple fallback pattern like "concept_id_123"
    should_convert <- FALSE
    
    if (grepl("_concept_id$", column, ignore.case = TRUE)) {
      # Column ends with "_concept_id", check if it's a simple fallback pattern
      # Simple fallback pattern: starts with "concept_id_" and has only digits after
      if (grepl("^concept_id_[0-9]+$", column, ignore.case = TRUE)) {
        # This is a simple numeric fallback like "concept_id_123", don't convert
        should_convert <- FALSE
      } else {
        # This is either an original concept_id column or a complex fallback
        # like "concept_id_123.value_as_concept_id", so convert it
        should_convert <- TRUE
      }
    }
    
    if (should_convert) {
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
