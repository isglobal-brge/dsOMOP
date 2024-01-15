#' @title Translate column concepts
#' @description This function translates the concepts in a specified column of a table using a dictionary of simplified concepts.
#' The dictionary is obtained from a database connection. Each concept in the column is replaced with its corresponding
#' name from the dictionary. If a concept does not exist in the dictionary or is NA, it is left unchanged.
#' @param connection A database connection object.
#' @param table A data frame representing the table in which the column concepts are to be translated.
#' @param columnName A string representing the name of the column to be translated.
#' @return A data frame with the specified column's concepts translated.
translateColumnConcepts <- function(connection, table, columnName) {
  # Get the simplified concept dictionary from the database connection
  dictionary <- getSimplifiedConceptsDictionary(connection)
  dictionary <- dictionary$concept
  for (i in 1:nrow(table)) {
    # Get the concept ID from the current row
    conceptId <- table[i, columnName]
    # Find the corresponding concept name in the dictionary
    conceptName <- dictionary[dictionary$concept_id == conceptId, "concept_name"]
    # If the concept name exists and is not NA, replace the concept ID with the concept name
    if (length(conceptName) > 0 && !is.na(conceptName[1])) {
      table[i, columnName] <- makeNames(conceptName[1])
    }
  }
  return(table)
}
