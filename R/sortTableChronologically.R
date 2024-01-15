#' @title Sort a table chronologically
#' @description This function sorts a table based on a specified time column. The table is rearranged in ascending order according to the values in the time column. This is useful when you want to analyze data in a chronological order.
#' @param table A data frame representing the table to be sorted.
#' @param timeColumn A character string representing the name of the time column in the table.
#' @return A data frame representing the sorted table.
sortTableChronologically <- function(table, timeColumn) {
  # Order the table based on the time column
  table <- table[order(table[[timeColumn]]), ]
  return(table)
}
