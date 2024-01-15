#' @title Add a new table to a base table
#' @description This function merges a new table with a base table based on a common ID column.
#' It first identifies the relationship between the new table and the base table, and then
#' identifies the ID column that they share. It also identifies any event and time columns
#' in the new table. If a time column is present, the new table is sorted chronologically.
#' If an event column is present, the event concepts are translated, the longitudinal data
#' is sequenced, and the table is reshaped. Finally, the new table is merged with the base
#' table using the common ID column.
#' @param connection A connection object to the database.
#' @param firstTable The base table to which the new table will be added.
#' @param secondTableName The name of the new table to be added.
#' @return A merged table consisting of the base table and the new table.
addTable <- function(connection, firstTable, secondTableName) {
  # Identify the relationship between the new table and the base table
  nextRelation <- getNextRelation(connection, secondTableName, "person")
  # Identify the ID column that the new table and the base table share
  idColumn <- paste0(nextRelation, "_id")
  # Identify any event column in the new table
  eventColumn <- paste0(strsplit(secondTableName, "_")[[1]][1], "_concept_id")
  secondTable <- getTable(connection, secondTableName)
  eventColumn <- if (eventColumn %in% names(secondTable)) eventColumn else NULL
  # Identify any time column in the new table
  timeColumn <- tryCatch(
    names(secondTable)[grepl("date", names(secondTable))][1],
    error = function(e) NULL
  )
  # If a time column is present, sort the new table chronologically
  if (!is.null(timeColumn)) {
    secondTable <- sortTableChronologically(secondTable, timeColumn)
  }
  # If an event column is present, translate the event concepts, sequence the longitudinal data, and reshape the table
  if (!is.null(eventColumn)) {
    secondTable <- translateColumnConcepts(connection, secondTable, eventColumn)
    secondTable <- sequenceLongitudinalData(secondTable, idColumn, eventColumn)
    secondTable <- reshapeTable(secondTable, idColumn, eventColumn)
  }
  # Merge the new table with the base table using the common ID column
  mergedTable <- mergeTables(firstTable, secondTable, idColumn)
  return(mergedTable)
}
