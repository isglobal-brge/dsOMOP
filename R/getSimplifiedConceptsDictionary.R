#' @title Get a simplified version of the concept dictionary
#' @description This function retrieves a dictionary from the database and simplifies its concept dictionary by retaining only the 'concept_id' and 'concept_name' columns.
#' The simplified dictionary is then returned. This function is useful when only the basic information about the concepts is required,
#' reducing the complexity and size of the dictionary.
#' @param connection A DBI connection object to the database.
#' @return A list where the concept dictionary is simplified to contain only 'concept_id' and 'concept_name'.
getSimplifiedConceptsDictionary <- function(connection) {
  # Retrieve the concept dictionary from the database
  conceptDictionary <- getDictionary(connection)
  # If the dictionary contains 'concept', simplify it by retaining only 'concept_id' and 'concept_name'
  if ("concept" %in% names(conceptDictionary)) {
    conceptDictionary$concept <- conceptDictionary$concept[, c("concept_id", "concept_name")]
  }
  return(conceptDictionary)
}
