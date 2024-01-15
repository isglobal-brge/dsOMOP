#' @title Get non-related ID column names
#' @description This function retrieves the names of columns that are not related to the specified table.
#' It does this by first getting all classified ID column names, and then filtering these based on the non-related ID categories.
#' @param connection A connection object to the database.
#' @param tableName The name of the table for which to retrieve non-related ID column names.
#' @return A character vector of non-related ID column names.
getNonRelatedIdColumnNames <- function(connection, tableName) {
  # Get all classified ID column names
  columnNames <- getClassifiedIdColumnNames(connection)
  # Get non-related ID categories for the specified table
  idCategories <- getNonRelatedIdCategories(connection, tableName)
  # Filter the column names based on the non-related ID categories
  columnNames <- columnNames[names(columnNames) %in% idCategories]
  return(columnNames)
}
