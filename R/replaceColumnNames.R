#' @title Replace column names in a dictionary using a table column
#' @description This function replaces the column names in a dictionary using a specific column from a table.
#' The function identifies the prefix and suffix of the dictionary column in the table column and uses these to construct new column names.
#' @param dictionary A named list or vector where the names are to be replaced.
#' @param tableColumn A character string representing the column in the table that is used to replace the dictionary column names.
#' @param dictionaryColumn A character string representing the column in the dictionary whose names are to be replaced.
#' @return A dictionary with replaced column names.
replaceColumnNames <- function(dictionary, tableColumn, dictionaryColumn) {
  # Identify the prefix and suffix of the dictionary column in the table column
  pre <- substr(tableColumn, 1, regexpr(dictionaryColumn, tableColumn) - 1)
  post <- substr(tableColumn, regexpr(dictionaryColumn, tableColumn) + nchar(dictionaryColumn), nchar(tableColumn))
  # Loop over the dictionary names and replace them with new names constructed from the prefix, dictionary name, and suffix
  for (i in 1:length(names(dictionary))) {
    renamedColumns <- paste0(pre, names(dictionary)[i], post)
    names(dictionary)[i] <- renamedColumns
  }
  return(dictionary)
}
