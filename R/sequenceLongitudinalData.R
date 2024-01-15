#' @title Sequence longitudinal data
#' @description This function sequences longitudinal data by numbering repeated variables over time for the same id and event type.
#' It modifies the event column of the input data table by appending a sequence number to the event name for repeated events.
#' @param dataTable A data frame representing the longitudinal data to be sequenced.
#' @param idColumn A character string representing the id column in the data table.
#' @param eventColumn A character string representing the event column in the data table.
#' @return A data frame with sequenced longitudinal data.
sequenceLongitudinalData <- function(dataTable, idColumn, eventColumn) {
  # Generate a sequence count for each event of the same type for the same id
  sequenceCount <- ave(seq_along(dataTable[[idColumn]]), dataTable[[idColumn]], dataTable[[eventColumn]], FUN = seq_along)
  # Modify the event column by appending the sequence count to the event name for repeated events
  dataTable[[eventColumn]] <- ifelse(sequenceCount > 1, paste0(dataTable[[eventColumn]], ".", sequenceCount), dataTable[[eventColumn]])
  # Identify rows with a sequence count of 2
  rowsWithTwo <- grepl("\\.2$", dataTable[[eventColumn]])
  # Identify equivalent rows (rows with the same event name but different sequence count)
  equivalentRows <- dataTable[[eventColumn]] == sub("\\.2$", "", dataTable[[eventColumn]][rowsWithTwo])
  # Modify the event column for equivalent rows by appending ".1" to the event name
  dataTable[[eventColumn]][equivalentRows] <- paste0(dataTable[[eventColumn]][equivalentRows], ".1")
  return(dataTable)
}
