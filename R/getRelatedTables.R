#' @title Retrieve related tables from a database connection.
#' @description This function retrieves the tables related to a given table from a database connection. It does this by first getting the table relations from the database, then it recursively explores the related tables, avoiding cycles by keeping track of already explored tables.
#' @param connection A DBI connection object to the database.
#' @param tableName The name of the table for which to retrieve related tables.
#' @return A vector of unique table names related to the input table.
getRelatedTables <- function(connection, tableName) {
  # Get the table relations from the database
  tableRelations <- getTableRelations(connection)
  exploredTables <- c()
  # Define a recursive function to explore related tables
  getRelatedTablesRecursive <- function(tableName, exploredTables) {
    # If the table has already been explored, return NULL to avoid cycles
    if (tableName %in% exploredTables) {
      return(NULL)
    }
    # Add the current table to the list of explored tables
    exploredTables <- c(exploredTables, tableName)
    # Get the tables related to the current table
    relatedTables <- tableRelations[[tableName]]
    # If there are no related tables, return NULL
    if (is.null(relatedTables)) {
      return(NULL)
    } else {
      # Otherwise, return the related tables and recursively explore their related tables
      return(c(relatedTables, unlist(lapply(relatedTables, function(x) getRelatedTablesRecursive(x, exploredTables)))))
    }
  }
  # Return the unique related tables for the input table
  return(unique(getRelatedTablesRecursive(tableName, exploredTables)))
}
