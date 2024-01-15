#' @export
mergeTables <- function(firstTable, secondTable, idColumn) {
  table <- merge(firstTable, secondTable, by = idColumn)
  table <- dsBase::mergeDS(
    x.name = "firstTable",
    y.name = "secondTable",
    by.x.names.transmit = idColumn,
    by.y.names.transmit = idColumn,
    all.x = TRUE,
    all.y = FALSE,
    sort = TRUE,
    suffixes.transmit = c(".x", ".y"),
    no.dups = TRUE,
    incomparables = NULL
  )
  return(table)
}
