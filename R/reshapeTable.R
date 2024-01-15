#' @export
reshapeTable <- function(table, idColumn, eventColumn) {
  table <- dsBase::reShapeDS(
    data.name = "table",
    varying.transmit = NULL,
    v.names.transmit = names(table)[!names(table) %in% c(idColumn, eventColumn)],
    timevar.name = eventColumn,
    idvar.name = idColumn,
    drop.transmit = NULL,
    direction = "wide",
    sep = "."
  )
  table <- moveColumnNameTokens(table)
  return(table)
}
