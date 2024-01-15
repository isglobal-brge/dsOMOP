#' @title Get ID categories from database tables
#' @description This function retrieves the names of all tables in the connected database and appends "_id" to each name.
#' The resulting list represents the categories of IDs available in the database.
#' @param connection A connection object to the database.
#' @return A character vector of ID categories.
getIdCategories <- function(connection) {
  # Retrieve the names of all tables in the database
  tableNames <- getTableNames(connection)
  # Append "_id" to each table name to form the ID categories
  idCategories <- paste0(tableNames, "_id")
  return(idCategories)
}
