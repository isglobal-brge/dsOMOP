#' @title Extend column in a table using a dictionary
#' @description This function extends a column in a table using a dictionary. It replaces the column names in the dictionary with the corresponding column in the table.
#' It then merges the table with the dictionary based on the common column. The function is used when extending non-sensitive values present in a table, not patient data.
#' Therefore, it does not require the use of the mergeDS function from dsBase.
#' @param table A data frame representing the table to be extended.
#' @param dictionary A data frame representing the dictionary used for extension.
#' @param tableColumn A character string representing the column in the table to be extended.
#' @param dictionaryColumn A character string representing the column in the dictionary used for extension.
#' @return A data frame representing the table after extension.
extendColumn <- function(table, dictionary, tableColumn, dictionaryColumn) {
  # Replace the column names in the dictionary with the corresponding column in the table
  dictionary <- replaceColumnNames(dictionary, tableColumn, dictionaryColumn)
  # Create a temporary merge column in the table
  table$tempMergeColumn <- table[[tableColumn]]
  # Merge the table with the dictionary based on the common column
  mergedTable <- merge(table, dictionary, by.x = "tempMergeColumn", by.y = tableColumn, all.x = TRUE)
  # Remove the temporary merge column from the merged table
  mergedTable$tempMergeColumn <- NULL
  return(mergedTable)
}
