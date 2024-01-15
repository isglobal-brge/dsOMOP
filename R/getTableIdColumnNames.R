#' @title Get ID column names from a table
#' @description This function retrieves the names of columns from a given table that contain '_id' in their names.
#' These are typically columns that serve as identifiers or keys in a relational database.
#' The function uses the 'grep' function to match the pattern '_id' in the column names of the table.
#' @param table A data frame or tibble representing the table from which to extract column names.
#' @return A character vector of column names from the input table that contain '_id'.
getTableIdColumnNames <- function(table) {
  # Get the names of all columns in the table
  columnNames <- colnames(table)
  # Filter the column names to include only those containing '_id'
  idColumnNames <- grep("_id", columnNames, value = TRUE)
  return(idColumnNames)
}
