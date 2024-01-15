#' @title Get unique IDs for a given column
#' @description This function retrieves unique IDs from a specified column in a database table.
#' The function connects to the database, constructs a SQL query to select distinct IDs from the specified column,
#' and executes the query. The IDs are then returned as a list.
#' @param connection A DBI connection object to the database.
#' @param columnName A string representing the name of the column in the format 'table.column'.
#' @return A list of unique IDs from the specified column.
getIdsForColumn <- function(connection, columnName) {
  # Split the 'table.column' format to separate table name and column name
  table <- strsplit(columnName, ".", fixed = TRUE)[[1]][1]
  column <- strsplit(columnName, ".", fixed = TRUE)[[1]][2]
  # Construct and execute the SQL query to get distinct IDs
  ids <- DBI::dbGetQuery(connection, paste0("SELECT DISTINCT ", column, " FROM ", table, " WHERE ", column, " IS NOT NULL"))
  return(ids[[1]])
}
