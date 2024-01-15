#' @title Classify ID column names
#' @description This function classifies the ID column names from a database connection based on their categories.
#' It retrieves the ID categories and foreign ID column names, then matches each ID column name to its category.
#' The result is a list where each element corresponds to a category and contains the ID column names belonging to that category.
#' @param connection A DBI connection object to the database.
#' @return A list of character vectors. Each element of the list corresponds to a category and contains the ID column names of that category.
getClassifiedIdColumnNames <- function(connection) {
  # Retrieve the ID categories from the database
  idCategories <- getIdCategories(connection)
  # Retrieve the foreign ID column names from the database
  idColumnNames <- getForeignIdColumnNames(connection)
  # Initialize an empty list to store the classified ID column names
  classifiedIdColumnNames <- list()
  for (tableName in names(idColumnNames)) {
    # Iterate over each ID column name in the table
    for (idColumnName in idColumnNames[[tableName]]) {
      # Match the ID column name to its category
      idCategory <- matchIdCategory(idColumnName, idCategories)
      # If the ID column name matches a category, add it to the list under that category
      if (!is.null(idCategory)) {
        classifiedIdColumnNames[[idCategory]] <- c(classifiedIdColumnNames[[idCategory]], idColumnName)
      }
    }
  }
  return(classifiedIdColumnNames)
}
