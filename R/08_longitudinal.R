#' Sequence Column in a Table
#'
#' This function adds a sequence number to rows within a table that have the same combination of concept ID column
#' and merge column values. It is particularly useful for distinguishing between multiple occurrences of the same
#' entity in longitudinal data. The sequence number is appended to the concept ID column for duplicates, starting
#' from the second occurrence.
#'
#' @param table A data frame representing the table to be processed.
#' @param conceptIdColumn A character string specifying the name of the column that contains concept IDs.
#' @param mergeColumn A character string specifying the name of the column used for merging.
#'
#' @return A data frame with sequence numbers appended to the concept ID column for rows that have the same
#' combination of concept ID column and merge column values.
#'
sequenceColumn <- function(table, conceptIdColumn, mergeColumn) {
  # Identifies rows with the same combination of concept ID column and merge column
  duplicatedRows <- table[duplicated(table[c(conceptIdColumn, mergeColumn)]) | duplicated(table[c(conceptIdColumn, mergeColumn)], fromLast = TRUE), ]

  if (nrow(duplicatedRows) > 0) {
    # Creates a sequence for each duplicate group
    table <- table %>%
      group_by(!!sym(conceptIdColumn), !!sym(mergeColumn)) %>%
      mutate(rowSequence = row_number()) %>%
      ungroup()

    # Appends the sequence number to the concept ID column for duplicates, starting from the second occurrence
    table <- table %>%
      mutate(!!sym(conceptIdColumn) := if_else(rowSequence > 1, paste0(!!sym(conceptIdColumn), ".", rowSequence), as.character(!!sym(conceptIdColumn)))) %>%
      select(-rowSequence)
  }

  return(table)
}
