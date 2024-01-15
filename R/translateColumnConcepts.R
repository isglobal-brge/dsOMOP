#' @export
translateColumnConcepts <- function(connection, table, columnName) {
  dictionary <- getSimplifiedConceptsDictionary(connection)
  dictionary <- dictionary$concept
  for (i in 1:nrow(table)) {
    conceptId <- table[i, columnName]
    conceptName <- dictionary[dictionary$concept_id == conceptId, "concept_name"]
    if (length(conceptName) > 0 && !is.na(conceptName[1])) {
      table[i, columnName] <- makeNames(conceptName[1])
    }
  }
  return(table)
}
