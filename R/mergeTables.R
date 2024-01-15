#' @title Merge tables based on a common identifier column
#' @description Merges two tables based on a common identifier column. The function is designed to be used when you have two tables with a common column and you want to merge them into a single table.
#' @param firstTable A data frame representing the first table to be merged.
#' @param secondTable A data frame representing the second table to be merged.
#' @param idColumn A character string representing the name of the common identifier column in both tables.
#' @return A merged data frame.
mergeTables <- function(firstTable, secondTable, idColumn) {
  # Use the mergeDS function from dsBase to merge the tables. This function inherits the disclosure control from DataSHIELD.
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
