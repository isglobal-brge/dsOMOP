#' @export
extendColumn <- function(table, dictionary, tableColumn, dictionaryColumn) {
  dictionary <- replaceColumnNames(dictionary, tableColumn, dictionaryColumn)
  table$tempMergeColumn <- table[[tableColumn]]
  mergedTable <- merge(table, dictionary, by.x = "tempMergeColumn", by.y = tableColumn, all.x = TRUE)
  mergedTable$tempMergeColumn <- NULL
  return(mergedTable)
}
