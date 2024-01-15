#' @export
addTables <- function(connection) {
  baseTable <- getTable(connection, "person")
  tableNames <- getRelatedTables(connection, "person")
  for (tableName in tableNames) {
    baseTable <- addTable(connection, baseTable, tableName)
  }
  return(baseTable)
}
