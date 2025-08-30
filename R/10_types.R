#' Convert Concept ID Columns to Factors in the Table
#'
#' @title Convert Concept ID Columns to Factors
#' @description
#' Converts original concept ID columns in a data frame to factors, while preserving
#' the data types of fallback columns created during concept translation failures.
#' This ensures proper handling of both translated and untranslated concept data.
#'
#' @details
#' The function applies intelligent logic to determine which columns should be converted to factors:
#' 
#' Conversion Rules:
#' - Columns ending with "_concept_id" are converted to factors
#' - Exception: Simple numeric fallback patterns like "concept_id_123" are preserved as numeric
#' 
#' Examples of conversion behavior:
#' - "condition_concept_id" -> factor (standard OMOP concept column)
#' - "concept_id_123" -> numeric (simple fallback, preserve original type)
#' - "concept_id_123.value_as_concept_id" -> factor (ends with _concept_id)
#' - "concept_id_123.value_as_number" -> numeric (does not end with _concept_id)
#' - "blood_pressure.value_as_concept_id" -> factor (ends with _concept_id)
#' - "blood_pressure.value_as_number" -> numeric (does not end with _concept_id)
#' 
#' This ensures that concept identifier columns maintain factor type for proper statistical
#' analysis, while preserving numeric data types for measurement values created during
#' the concept translation fallback mechanism.
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
#' # Example demonstrating conversion rules
#' data <- data.frame(
#'   person_id = 1:3,
#'   condition_concept_id = c(1234, 5678, 1234),                      # -> factor (ends with _concept_id)
#'   concept_id_123 = c(1.5, 2.3, 3.1),                               # -> numeric (simple fallback pattern)
#'   concept_id_123.value_as_concept_id = c("A", "B", "C"),           # -> factor (ends with _concept_id)
#'   concept_id_123.value_as_number = c(10.1, 20.2, 30.3),            # -> numeric (ends with _as_number)
#'   blood_pressure.value_as_concept_id = c("normal", "hypertension", # -> factor (ends with _concept_id)
#'                                          "hypotension"),            
#'   blood_pressure.value_as_number = c(120.0, 180.5, 90.2)           # -> numeric (ends with _as_number)
#' )
#' 
#' result <- conceptsToFactors(data)
#' # Only columns ending with "_concept_id" become factors
#' # Simple numeric fallback patterns like "concept_id_123" remain numeric
#' }
#'
conceptsToFactors <- function(table) {
  # Step 1: Iterate through all columns in the table
  for (column in names(table)) {
    
    # Step 2: Apply conversion logic based on column name patterns
    # Rule: Convert to factor ONLY if column ends with "_concept_id"
    # Exception: Preserve simple numeric fallback patterns like "concept_id_123"
    should_convert <- FALSE
    
    if (grepl("_concept_id$", column, ignore.case = TRUE)) {
      # Column ends with "_concept_id" - candidate for conversion
      
      # Check for simple numeric fallback pattern: "concept_id_[digits]"
      # Examples: concept_id_123, concept_id_456, CONCEPT_ID_789
      if (grepl("^concept_id_[0-9]+$", column, ignore.case = TRUE)) {
        # Simple fallback pattern - preserve original type (usually numeric)
        # These represent raw numeric values from failed concept translations
        should_convert <- FALSE
      } else {
        # All other "_concept_id" columns should be factors:
        # - Standard OMOP columns: "condition_concept_id", "drug_concept_id"
        # - Complex fallbacks: "concept_id_123.value_as_concept_id", "blood_pressure.value_as_concept_id"
        should_convert <- TRUE
      }
    }
    # Note: Columns NOT ending with "_concept_id" are never converted
    # Examples that remain unchanged: "concept_id_123.value_as_number", "blood_pressure.value_as_number"
    
    if (should_convert) {
      # Step 3: Convert to factor for proper statistical analysis
      table[[column]] <- as.factor(table[[column]])
    }
  }
  
  # Step 4: Return table with appropriate factor conversions
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
