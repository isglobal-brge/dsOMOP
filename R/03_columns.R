#' Filter Columns from a Table
#'
#' @title Column Filtering with Mandatory Column Support
#' @description
#' Filters columns from a given table based on specified criteria while preserving mandatory columns.
#' Provides flexible column selection with built-in handling of source value columns.
#'
#' @details
#' The function operates in two modes:
#' 1. Default mode: When no specific columns are selected, automatically excludes columns containing '_source'
#' 2. Selective mode: Filters based on provided column names while ensuring mandatory columns are retained
#'
#' The filtering process follows these steps:
#' 1. Checks if specific columns are requested
#' 2. If not, applies default source column exclusion
#' 3. Merges mandatory columns with selected columns
#' 4. Validates final column selection against available table columns
#'
#' @param table The input data frame or tibble to filter columns from
#' @param tableColumns Character vector containing all available column names in the table
#' @param selectedColumns Character vector specifying which columns to include. If NULL, applies default filtering
#' @param keepColumns Character vector of mandatory columns that must be retained regardless of other filters
#'
#' @return Character vector containing the filtered column names
#'
#' @examples
#' \dontrun{
#' # Example table with various columns
#' myTable <- data.frame(
#'   id = 1:3,
#'   name = c("A", "B", "C"),
#'   value_source = c("X", "Y", "Z"),
#'   status = c(TRUE, FALSE, TRUE)
#' )
#'
#' # Get all columns except source columns
#' cols <- filterColumns(myTable, 
#'                      names(myTable), 
#'                      selectedColumns = NULL,
#'                      keepColumns = NULL)
#'
#' # Select specific columns while keeping mandatory ones
#' cols <- filterColumns(myTable,
#'                      names(myTable),
#'                      selectedColumns = c("name", "status"),
#'                      keepColumns = "id")
#' }
#'
filterColumns <- function(table, tableColumns, selectedColumns, keepColumns) {
  # Step 1: Handle default filtering case
  if (is.null(selectedColumns)) {
    # Exclude columns containing '_source' in their names
    selectedColumns <- tableColumns[!grepl("_source", tableColumns)]
    return(selectedColumns)
  }

  # Step 2: Merge mandatory columns with selected columns
  if (!is.null(keepColumns)) {
    # Only include keepColumns that actually exist in the table
    validKeepColumns <- keepColumns[keepColumns %in% tableColumns]
    selectedColumns <- unique(c(selectedColumns, validKeepColumns))
  }

  # Step 3: Filter and validate final column selection
  filteredColumns <- tableColumns[tableColumns %in% selectedColumns]
  return(filteredColumns)
}

#' Generate Concept ID Column Name
#'
#' @title Dynamic Concept ID Column Name Generation
#' @description
#' Generates standardized concept ID column names for OMOP CDM tables by applying
#' consistent naming conventions and handling special table name cases.
#'
#' @details
#' The function performs the following transformations:
#' 1. Converts table name to lowercase for consistency
#' 2. Removes specific suffixes ('_occurrence', '_exposure', '_era', '_strength')
#' 3. Appends '_concept_id' to create the final column name
#'
#' This standardization is crucial for:
#' * Maintaining consistent column naming across the OMOP CDM
#' * Supporting automated concept ID column identification
#' * Ensuring compatibility with standard OMOP CDM tools
#'
#' @param tableName Character string containing the OMOP CDM table name
#'
#' @return Character string representing the standardized concept ID column name
#'
#' @examples
#' \dontrun{
#' # Basic table name
#' getConceptIdColumn("condition")  # Returns "condition_concept_id"
#'
#' # Table with occurrence suffix
#' getConceptIdColumn("drug_exposure")  # Returns "drug_concept_id"
#'
#' # Case-insensitive handling
#' getConceptIdColumn("OBSERVATION")  # Returns "observation_concept_id"
#' }
#'
getConceptIdColumn <- function(tableName) {
  # Step 1: Standardize case
  tableName <- tolower(tableName)
  
  # Step 2: Remove special suffixes
  baseName <- gsub("_occurrence", "", tableName)
  baseName <- gsub("_exposure", "", baseName)
  baseName <- gsub("_era", "", baseName)
  baseName <- gsub("_strength", "", baseName)
  
  # Step 3: Generate concept ID column name
  conceptIdColumn <- paste0(baseName, "_concept_id")
  return(conceptIdColumn)
}

#' Case-Insensitive Column Name Matching
#'
#' @title Find Column Names with Case-Insensitive Matching
#' @description
#' Performs robust column name matching with support for both exact and case-insensitive
#' matches. Prioritizes exact matches before falling back to case-insensitive comparison.
#'
#' @details
#' The matching process follows this hierarchy:
#' 1. Attempts exact string matching first
#' 2. If no exact match is found, performs case-insensitive matching
#' 3. Returns NULL if no match is found at either level
#'
#' This approach is particularly useful for:
#' * Supporting flexible column name references
#' * Maintaining backward compatibility
#' * Handling inconsistent column name casing
#'
#' @param columnNames Character vector of available column names to search through
#' @param target Character string of the target column name to find
#'
#' @return Character string of the matched column name or NULL if no match found
#'
#' @examples
#' \dontrun{
#' cols <- c("PersonId", "BirthDate", "Gender")
#'
#' # Exact match
#' findCaseInsensitiveColumn(cols, "PersonId")  # Returns "PersonId"
#'
#' # Case-insensitive match
#' findCaseInsensitiveColumn(cols, "personid")  # Returns "PersonId"
#'
#' # No match
#' findCaseInsensitiveColumn(cols, "Age")       # Returns NULL
#' }
#'
findCaseInsensitiveColumn <- function(columnNames, target) {
  # Step 1: Initialize result
  caseInsensitiveColumn <- NULL

  # Step 2: Try exact match first
  exactMatchIndex <- match(target, columnNames)
  if (!is.na(exactMatchIndex)) {
    caseInsensitiveColumn <- columnNames[exactMatchIndex]
  } else {
    # Step 3: Attempt case-insensitive match
    lowerColumnNames <- tolower(columnNames)
    lowerTarget <- tolower(target)
    matchIndex <- match(lowerTarget, lowerColumnNames)

    # Step 4: Return original column name if case-insensitive match found
    if (!is.na(matchIndex)) {
      caseInsensitiveColumn <- columnNames[matchIndex]
    }
  }

  return(caseInsensitiveColumn)
}
