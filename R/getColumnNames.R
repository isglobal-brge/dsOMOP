#' @export
getColumnNames <- function(connection) {
  tableNames <- getTableNames(connection)
  columnNames <- list()
  for (tableName in tableNames) {
    columns <- DBI::dbListFields(connection, tableName)
    columnNames[[tableName]] <- paste(tableName, columns, sep = ".")
  }
  return(columnNames)
}
