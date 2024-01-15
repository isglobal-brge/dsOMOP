#' @export
getNonRelatedTables <- function(connection, tableName) {
  tableNames <- getTableNames(connection)
  relatedTables <- getRelatedTables(connection, tableName)
  nonRelatedTables <- tableNames[!tableNames %in% c(relatedTables, tableName)]
  return(nonRelatedTables)
}
