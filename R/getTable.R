#' @title Retrieve a table from the database
#' @description This function retrieves a specified table from the database connection, excluding any columns named 'source'.
#' @param connection A DBI connection object to the database.
#' @param tableName The name of the table to be retrieved.
#' @return A data frame representing the table, excluding any 'source' columns.
getTable <- function(connection, tableName) {
  # Get all column names from the table
  allColumns <- DBI::dbListFields(connection, tableName)
  # Exclude any columns named 'source'
  selectedColumns <- allColumns[!grepl("source", allColumns)]
  # Create a SQL query to select only the desired columns
  query <- paste0("SELECT ", paste(selectedColumns, collapse = ", "), " FROM ", tableName)
  # Execute the query and return the result
  table <- DBI::dbGetQuery(connection, query)
  return(table)
}
