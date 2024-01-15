#' @title Get table names
#' @description This function retrieves the names of all tables in the connected database.
#' @param connection A connection object to the database.
#' @return A character vector of table names.
getTableNames <- function(connection) {
  # Use the dbListTables function from the DBI package to get the table names
  tables <- DBI::dbListTables(connection)
  return(tables)
}
