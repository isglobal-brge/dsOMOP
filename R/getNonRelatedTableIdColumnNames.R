#' @title Get non-related table ID column names
#' @description This function retrieves the names of ID columns from a specified table that are not related to it.
#' @param connection A connection object to the database.
#' @param table A character string specifying the name of the table from which to retrieve ID column names.
#' @param nonRelatedTable A character string specifying the name of the non-related table.
#' @return A character vector of non-related ID column names.
getNonRelatedTableIdColumnNames <- function(connection, table, nonRelatedTable) {
  # Get the ID column names from the specified table
  idColumnNames <- getTableIdColumnNames(table)
  # Get the ID categories from the non-related table
  idCategories <- getNonRelatedIdCategories(connection, nonRelatedTable)
  # Filter the ID column names to only include those that are not related to the non-related table
  nonRelatedIdColumnNames <- idColumnNames[sapply(idColumnNames, function(idColumnName) any(sapply(idCategories, function(idCategory) grepl(idCategory, idColumnName))))]
  return(nonRelatedIdColumnNames)
}
