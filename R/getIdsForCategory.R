#' @title Retrieve unique IDs for a given category
#' @description This function retrieves unique IDs for a given category from the database.
#' It does so by iterating over each column name in the category, fetching the IDs for each column,
#' and then returning a unique set of these IDs.
#' @param connection A DBI connection object to the database.
#' @param idCategory A character vector of column names representing an ID category.
#' @return A vector of unique IDs for the given category.
getIdsForCategory <- function(connection, idCategory) {
  # Fetch IDs for each column in the category
  ids <- unique(unlist(lapply(idCategory, function(columnName) {
    getIdsForColumn(connection, columnName)
  })))
  return(ids)
}
