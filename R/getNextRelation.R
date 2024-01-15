#' @title Get the next relation in the path between two tables
#' @description This function retrieves the next step in the relations between a given table and a target table.
#' It is useful for determining the next table to access via IDs when moving towards the target table.
#' @param connection A DBI connection object to the database.
#' @param tableName A string representing the name of the starting table.
#' @param targetTableName A string representing the name of the target table.
#' @return The name of the next table in the relation path from the starting table to the target table.
getNextRelation <- function(connection, tableName, targetTableName) {
  # Retrieve the relation path between the starting table and the target table
  relationPath <- getRelationPath(connection, tableName, targetTableName)
  # Return the name of the next table in the relation path
  return(relationPath[1])
}
