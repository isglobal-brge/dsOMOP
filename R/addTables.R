#' @title Add all tables related to 'person'
#' @description This function retrieves all tables related to the 'person' table from the database and adds them to the base table.
#' The base table is initially the 'person' table and is updated iteratively with each related table.
#' The function uses the `addTable` function to merge each related table with the current base table.
#' @param connection A connection object to the database.
#' @return A data frame that is the result of merging the base table with all its related tables.
addTables <- function(connection) {
  # Retrieve the 'person' table from the database
  baseTable <- getTable(connection, "person")
  # Get the names of all tables related to the 'person' table
  tableNames <- getRelatedTables(connection, "person")
  for (tableName in tableNames) {
    # Add the current related table to the base table
    baseTable <- addTable(connection, baseTable, tableName)
  }
  return(baseTable)
}
