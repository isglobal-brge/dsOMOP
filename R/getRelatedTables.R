#' @export
getRelatedTables <- function(connection, tableName) {
  tableRelations <- getTableRelations(connection)
  exploredTables <- c()
  getRelatedTablesRecursive <- function(tableName, exploredTables) {
    if (tableName %in% exploredTables) {
      return(NULL)
    }
    exploredTables <- c(exploredTables, tableName)
    relatedTables <- tableRelations[[tableName]]
    if (is.null(relatedTables)) {
      return(NULL)
    } else {
      return(c(relatedTables, unlist(lapply(relatedTables, function(x) getRelatedTablesRecursive(x, exploredTables)))))
    }
  }
  return(unique(getRelatedTablesRecursive(tableName, exploredTables)))
}
