#' @title Retrieve a table from the database
#' @description This function retrieves a specified table from the database connection, excluding any columns named 'source'.
#' @param connection A DBI connection object to the database.
#' @param tableName The name of the table to be retrieved.
#' @return A data frame representing the table, excluding any 'source' columns.
getTable <- function(connection, tableName) {
  # Read the table from the database
  table <- DBI::dbReadTable(connection, tableName)
  # Exclude any columns named 'source'
  table <- table[, !grepl("source", names(table))]
  return(table)
}
