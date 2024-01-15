#' @export
replaceColumnNames <- function(dictionary, tableColumn, dictionaryColumn) {
  pre <- substr(tableColumn, 1, regexpr(dictionaryColumn, tableColumn) - 1)
  post <- substr(tableColumn, regexpr(dictionaryColumn, tableColumn) + nchar(dictionaryColumn), nchar(tableColumn))
  for (i in 1:length(names(dictionary))) {
    renamedColumns <- paste0(pre, names(dictionary)[i], post)
    names(dictionary)[i] <- renamedColumns
  }
  return(dictionary)
}
