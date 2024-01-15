#' @title Retrieve non-related tables
#' @description This function retrieves the names of tables that are not related to the specified table in the database.
#' It first fetches all table names from the database, then identifies the tables related to the specified table.
#' The function finally returns the names of tables that are neither related to the specified table nor the specified table itself.
#' @param connection A DBI connection object to the database.
#' @param tableName A string representing the name of the table.
#' @return A character vector of table names that are not related to the specified table.
getNonRelatedTables <- function(connection, tableName) {
  # Fetch all table names from the database
  tableNames <- getTableNames(connection)
  # Identify tables related to the specified table
  relatedTables <- getRelatedTables(connection, tableName)
  # Return the names of tables that are not related to the specified table
  nonRelatedTables <- tableNames[!tableNames %in% c(relatedTables, tableName)]
  return(nonRelatedTables)
}
