#' @title Get column names from database tables
#' @description This function retrieves the column names from all tables in a database connection.
#' It returns a list where each element is a vector of column names, prefixed with the table name.
#' @param connection A DBI connection object to the database.
#' @return A list of character vectors. Each element of the list corresponds to a table in the database,
#' and contains the column names of that table, each prefixed with the table name.
getColumnNames <- function(connection) {
  # Get the names of all tables in the database
  tableNames <- getTableNames(connection)
  columnNames <- list()
  for (tableName in tableNames) {
    # Get the column names for the current table
    columns <- DBI::dbListFields(connection, tableName)
    # Prefix each column name with the table name and store in the list
    columnNames[[tableName]] <- paste(tableName, columns, sep = ".")
  }
  return(columnNames)
}
