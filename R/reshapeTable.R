#' @title Reshape a table from long to wide format
#' @description This function reshapes a table from long to wide format using the reShapeDS function from the dsBase package.
#' The reshaping process is controlled by specifying the id and event columns. The remaining columns are considered as varying variables.
#' The function also moves the column name tokens to make the reshaped table more understandable.
#' @param table A data frame representing the table to be reshaped.
#' @param idColumn A character string representing the id column in the table.
#' @param eventColumn A character string representing the event column in the table.
#' @return A data frame representing the reshaped table.
reshapeTable <- function(table, idColumn, eventColumn) {
  # Use the reShapeDS function from dsBase to reshape the table. This function inherits the disclosure control from DataSHIELD.
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
  # Move the column name tokens to make the reshaped table more understandable.
  table <- moveColumnNameTokens(table)
  return(table)
}
