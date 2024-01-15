#' @export
addTable <- function(connection, firstTable, secondTableName) {
  nextRelation <- getNextRelation(connection, secondTableName, "person")
  idColumn <- paste0(nextRelation, "_id")

  eventColumn <- paste0(strsplit(secondTableName, "_")[[1]][1], "_concept_id")
  secondTable <- getTable(connection, secondTableName)
  eventColumn <- if (eventColumn %in% names(secondTable)) eventColumn else NULL

  timeColumn <- tryCatch(
    names(secondTable)[grepl("date", names(secondTable))][1],
    error = function(e) NULL
  )

  if (!is.null(timeColumn)) {
    secondTable <- sortTableChronologically(secondTable, timeColumn)
  }

  if (!is.null(eventColumn)) {
    secondTable <- translateColumnConcepts(connection, secondTable, eventColumn)
    secondTable <- sequenceLongitudinalData(secondTable, idColumn, eventColumn)
    secondTable <- reshapeTable(secondTable, idColumn, eventColumn)
  }

  mergedTable <- mergeTables(firstTable, secondTable, idColumn)
  return(mergedTable)
}
