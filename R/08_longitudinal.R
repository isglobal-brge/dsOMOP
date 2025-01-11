#' Add Sequential Numbering to Duplicate Entries in Longitudinal Data
#'
#' @title Sequence Numbering for Longitudinal Data
#' @description
#' Adds sequential numbers to rows that share identical values across specified columns
#' in longitudinal data. This function is essential for distinguishing between multiple
#' occurrences of the same entity over time by appending sequence numbers to the
#' concept ID values.
#'
#' @details
#' The function performs the following key operations:
#' 1. Duplicate Detection:
#'    * Identifies rows sharing the same concept ID and merge column values
#'    * Checks for duplicates from both directions (start and end)
#'    * Only processes data if duplicates are found
#'
#' 2. Sequence Generation:
#'    * Groups data by concept ID and merge column
#'    * Assigns sequential numbers within each group
#'    * Maintains original order of entries
#'
#' 3. Value Modification:
#'    * Appends sequence numbers to concept IDs using dot notation
#'    * Preserves original values for non-duplicate entries
#'    * Removes temporary sequence column
#'
#' Common Use Cases:
#' * Longitudinal patient data with multiple visits
#' * Time-series measurements of the same concept
#' * Sequential medical procedures or observations
#'
#' @param table A data frame containing the longitudinal data to be processed
#' @param conceptIdColumn A character string specifying the column name containing
#'        concept identifiers that need sequential numbering
#' @param mergeColumn A character string specifying the column name used in
#'        combination with conceptIdColumn to identify unique entries
#'
#' @return A data frame with modified concept IDs, where duplicate entries have
#'         sequential numbers appended (e.g., "concept_1.1", "concept_1.2")
#'
#' @examples
#' \dontrun{
#' # Example with patient visit data
#' visits <- data.frame(
#'   patient_id = c(1, 1, 1, 2, 2),
#'   visit_concept = c("checkup", "checkup", "checkup", "surgery", "surgery"),
#'   visit_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01",
#'                         "2023-01-15", "2023-02-15"))
#' )
#'
#' # Add sequence numbers to duplicate visits
#' sequenced_visits <- sequenceColumn(
#'   table = visits,
#'   conceptIdColumn = "visit_concept",
#'   mergeColumn = "patient_id"
#' )
#' # Results in visit_concept values: "checkup.1", "checkup.2", "checkup.3", "surgery.1", "surgery.2"
#' }
#'
sequenceColumn <- function(table, conceptIdColumn, mergeColumn) {
  # Step 1: Identify duplicate entries
  # Find rows that have matching values in both conceptIdColumn and mergeColumn
  # Check duplicates from both directions to catch all instances
  duplicatedRows <- table[duplicated(table[c(conceptIdColumn, mergeColumn)]) |
                         duplicated(table[c(conceptIdColumn, mergeColumn)], fromLast = TRUE), ]

  # Step 2: Process duplicates if they exist
  if (nrow(duplicatedRows) > 0) {
    # Generate sequential numbers for each group of duplicates
    table <- table %>%
      # Group by both columns to identify unique combinations
      group_by(!!sym(conceptIdColumn), !!sym(mergeColumn)) %>%
      # Add sequence numbers within each group
      mutate(rowSequence = row_number()) %>%
      # Remove grouping to prevent affecting subsequent operations
      ungroup()

    # Step 3: Modify concept IDs for duplicate entries
    table <- table %>%
      # Append sequence numbers to concept IDs using dot notation
      mutate(!!sym(conceptIdColumn) := paste0(!!sym(conceptIdColumn), ".", rowSequence)) %>%
      # Remove the temporary sequence column
      select(-rowSequence)
  }

  return(table)
}
