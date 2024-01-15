#' @export
sequenceLongitudinalData <- function(dataTable, idColumn, eventColumn) {
  sequenceCount <- ave(seq_along(dataTable[[idColumn]]), dataTable[[idColumn]], dataTable[[eventColumn]], FUN = seq_along)
  dataTable[[eventColumn]] <- ifelse(sequenceCount > 1, paste0(dataTable[[eventColumn]], ".", sequenceCount), dataTable[[eventColumn]])
  rowsWithTwo <- grepl("\\.2$", dataTable[[eventColumn]])
  equivalentRows <- dataTable[[eventColumn]] == sub("\\.2$", "", dataTable[[eventColumn]][rowsWithTwo])
  dataTable[[eventColumn]][equivalentRows] <- paste0(dataTable[[eventColumn]][equivalentRows], ".1")
  return(dataTable)
}
