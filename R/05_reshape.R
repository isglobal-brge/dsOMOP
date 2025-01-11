#' Reshape OMOP CDM Table to Wide Format
#'
#' @title Wide Format Reshaping for OMOP CDM Tables
#' @description
#' Transforms an OMOP CDM table from long to wide format to facilitate merging with other tables
#' in the database. The function handles concept standardization, longitudinal data sequencing,
#' and column name reorganization.
#'
#' @details
#' The function performs several key operations:
#' 1. Validates the presence of required merge column
#' 2. Optionally sequences longitudinal data to handle repeated measurements
#' 3. Reshapes the table using DataSHIELD's reShapeDS function
#' 4. Standardizes column names for consistency and readability
#'
#' The reshaping process uses:
#' * Merge column (typically person_id) as the identifier variable
#' * Concept ID column as the time variable to characterize element types
#' * Remaining columns as measurement variables
#'
#' This transformation is essential for:
#' * Creating person-centric views of the data
#' * Facilitating joins between OMOP CDM tables
#' * Handling repeated measurements properly
#' * Maintaining data integrity during transformations
#'
#' @param table A data frame containing OMOP CDM data to be reshaped
#' @param conceptIdColumn Character string specifying the concept ID column name that defines element types
#' @param mergeColumn Character string specifying the identifier column name (usually "person_id")
#' @param sequenceLongitudinal Logical indicating whether to sequence repeated measurements (default: FALSE)
#' 
#' @return A data frame in wide format where:
#' * Rows represent unique entities (e.g., persons)
#' * Columns combine concept types with original measurements
#' * Column names are standardized and properly sequenced
#'
#' @examples
#' \dontrun{
#' # Basic reshaping of a condition table
#' condition_data <- data.frame(
#'   person_id = c(1, 1, 2),
#'   condition_concept_id = c("diabetes", "hypertension", "diabetes"),
#'   start_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
#' )
#' 
#' # Reshape without longitudinal sequencing
#' wide_data <- reshapeTable(
#'   table = condition_data,
#'   conceptIdColumn = "condition_concept_id",
#'   mergeColumn = "person_id"
#' )
#'
#' # Reshape with longitudinal sequencing
#' wide_data_seq <- reshapeTable(
#'   table = condition_data,
#'   conceptIdColumn = "condition_concept_id",
#'   mergeColumn = "person_id",
#'   sequenceLongitudinal = TRUE
#' )
#' }
#'
reshapeTable <- function(table, conceptIdColumn, mergeColumn, sequenceLongitudinal = FALSE) {
  # Step 1: Validate merge column presence
  if (!mergeColumn %in% names(table)) {
    stop(paste0("The column '", mergeColumn, "' is not present in the table."))
  }

  # Step 2: Handle longitudinal data sequencing if requested
  if(sequenceLongitudinal) {
    table <- sequenceColumn(table, conceptIdColumn, mergeColumn)
  }

  # Step 3: Perform wide format reshaping
  # Identify measurement variables (all columns except merge and concept columns)
  measureVars <- names(table)[!names(table) %in% c(mergeColumn, conceptIdColumn)]
  
  # Execute reshape operation using DataSHIELD
  table <- dsBase::reShapeDS(
    data.name = "table",
    varying.transmit = NULL,
    v.names.transmit = measureVars,
    timevar.name = conceptIdColumn,
    idvar.name = mergeColumn,
    drop.transmit = NULL,
    direction = "wide",
    sep = "."
  )

  # Step 4: Standardize column naming structure
  table <- rearrangeColumnNames(table)
  return(table)
}

#' Rearrange Column Names for Reshaped OMOP CDM Tables
#'
#' @title Column Name Standardization for Reshaped Tables
#' @description
#' Standardizes and rearranges column names in reshaped OMOP CDM tables to improve readability
#' and maintain consistent naming conventions. Handles the reorganization of composite column
#' names created during the reshaping process.
#'
#' @details
#' The function performs the following operations:
#' 1. Splits column names into constituent tokens
#' 2. Reorders tokens to prioritize concept information
#' 3. Reconstructs column names with standardized structure
#'
#' Column name transformation process:
#' * Original format: "measurement.concept"
#' * Transformed format: "concept.measurement"
#'
#' This standardization is important for:
#' * Maintaining consistent naming conventions
#' * Improving column name readability
#' * Facilitating data analysis and filtering
#' * Supporting automated processing
#'
#' @param table A data frame containing reshaped OMOP CDM data with composite column names
#'
#' @return A data frame with standardized column names following the concept-first convention
#'
#' @examples
#' \dontrun{
#' # Example with reshaped data
#' df <- data.frame(
#'   person_id = 1:3,
#'   value.diabetes = c(1, 0, 1),
#'   date.diabetes = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
#' )
#'
#' # Standardize column names
#' df_standard <- rearrangeColumnNames(df)
#' # Results in: person_id, diabetes.value, diabetes.date
#' }
#'
rearrangeColumnNames <- function(table) {
  # Step 1: Split column names into tokens
  tokens <- strsplit(names(table), "\\.")

  # Step 2: Rearrange tokens to prioritize concept information
  rearrangedTokens <- lapply(tokens, function(token) {
    if (length(token) > 1) {
      # Move measurement type to end, bringing concept to front
      token <- c(token[-1], token[1])
    }
    return(token)
  })

  # Step 3: Reconstruct column names with standardized structure
  newColumns <- sapply(rearrangedTokens, paste, collapse = ".")
  names(table) <- newColumns
  return(table)
}
