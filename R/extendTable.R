#' @export
extendTable <- function(connection, table, maxDepth = 5) {
  dictionary <- getSimplifiedConceptsDictionary(connection)
  columnNames <- getNonRelatedTableIdColumnNames(connection, table, "person")
  extendedColumns <- list()
  depth <- 0
  while (length(columnNames) > 0 && depth < maxDepth) {
    for (columnName in columnNames) {
      for (name in names(dictionary)) {
        if (grepl(name, columnName)) {
          dictionaryColumnName <- paste0(name, "_id")
          table <- extendColumn(table, dictionary[[name]], columnName, dictionaryColumnName)
          extendedColumns <- c(extendedColumns, columnName)
        }
      }
    }
    columnNames <- setdiff(getNonRelatedTableIdColumnNames(connection, table, "person"), extendedColumns)
    depth <- depth + 1
  }
  return(table)
}
