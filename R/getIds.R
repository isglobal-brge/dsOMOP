#' @title Retrieve IDs for each category
#' @description This function retrieves the IDs for each category from the 'person' table in the given connection.
#' It uses the helper function 'getNonRelatedIdColumnNames' to get the column names that are not related to the 'person' table.
#' Then, it iterates over these column names (categories) and uses the 'getIdsForCategory' function to retrieve the IDs for each category.
#' The IDs are stored in a list where each element corresponds to a category.
#' @param connection The connection to the database.
#' @return A list of IDs for each category.
getIds <- function(connection) {
  # Get the column names that are not related to the 'person' table
  idColumnNames <- getNonRelatedIdColumnNames(connection, "person")
  ids <- list()
  for (idCategory in names(idColumnNames)) {
    # Retrieve the IDs for each category and store them in the list
    ids[[idCategory]] <- getIdsForCategory(connection, idColumnNames[[idCategory]])
  }
  return(ids)
}
