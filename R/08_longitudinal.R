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

