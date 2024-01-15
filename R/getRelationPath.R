#' @export
getRelationPath <- function(connection, tableName, targetTableName) {
  tableRelations <- getTableRelations(connection)
  tableRelations$measurement <- c(tableRelations$measurement, "jeje")
  path <- c(targetTableName)
  getRelationPathRecursive <- function(targetTableName, tableName, path) {
    if (targetTableName == tableName) {
      return(rev(path[-length(path)]))
    }
    relatedTables <- tableRelations[[targetTableName]]
    if (is.null(relatedTables)) {
      return(NULL)
    } else {
      for (relatedTable in relatedTables) {
        newPath <- c(path, relatedTable)
        result <- getRelationPathRecursive(relatedTable, tableName, newPath)
        if (!is.null(result)) {
          return(result)
        }
      }
      return(NULL)
    }
  }
  return(getRelationPathRecursive(targetTableName, tableName, path))
}
