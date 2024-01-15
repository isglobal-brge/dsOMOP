#' @title Retrieve the path of relations between two tables
#' @description This function retrieves the path of relations between a source table and a target table from a database connection. It does this by first getting the table relations from the database, then it recursively explores the related tables, avoiding cycles by keeping track of already explored tables. If the target table is reached, it returns the path of relations. If the target table cannot be reached, it returns NULL.
#' @param connection A DBI connection object to the database.
#' @param tableName The name of the source table.
#' @param targetTableName The name of the target table.
#' @return A vector of table names representing the path of relations from the source table to the target table, or NULL if no such path exists.
getRelationPath <- function(connection, tableName, targetTableName) {
  # Get the table relations from the database
  tableRelations <- getTableRelations(connection)
  # Initialize the path with the target table name
  path <- c(targetTableName)
  # Define a recursive function to explore related tables and find the path of relations
  getRelationPathRecursive <- function(targetTableName, tableName, path) {
    # If the target table is reached, return the path of relations
    if (targetTableName == tableName) {
      return(rev(path[-length(path)]))
    }
    # Get the tables related to the current target table
    relatedTables <- tableRelations[[targetTableName]]
    # If there are no related tables, return NULL
    if (is.null(relatedTables)) {
      return(NULL)
    } else {
      # Otherwise, for each related table, recursively explore its related tables
      for (relatedTable in relatedTables) {
        newPath <- c(path, relatedTable)
        result <- getRelationPathRecursive(relatedTable, tableName, newPath)
        # If a path of relations is found, return it
        if (!is.null(result)) {
          return(result)
        }
      }
      # If no path of relations is found, return NULL
      return(NULL)
    }
  }
  # Start the recursive exploration of related tables
  return(getRelationPathRecursive(targetTableName, tableName, path))
}
