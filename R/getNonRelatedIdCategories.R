#' @title Get non-related ID categories from a given table
#' @description This function retrieves the categories of non-related IDs from a specified table in the database.
#' It first identifies the non-related tables by calling the `getNonRelatedTables` function, and then appends "_id" to each table name to form the category names.
#' @param connection A connection object to the database.
#' @param tableName The name of the table from which to retrieve non-related ID categories.
#' @return A character vector of non-related ID category names.
getNonRelatedIdCategories <- function(connection, tableName) {
  # Get the names of non-related tables
  nonRelatedTables <- getNonRelatedTables(connection, tableName)
  # Append "_id" to each table name to form the category names
  nonClinicalDataCategories <- paste0(nonRelatedTables, "_id")
  return(nonClinicalDataCategories)
}
