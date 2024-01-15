#' @title Retrieve ID column names from database tables
#' @description This function retrieves the names of ID columns from all tables in a database connection.
#' It returns a list where each element is a vector of ID column names, excluding any columns named 'source'.
#' @param connection A DBI connection object to the database.
#' @return A list of character vectors. Each element of the list corresponds to a table in the database,
#' and contains the ID column names of that table, excluding 'source' columns.
getIdColumnNames <- function(connection) {
  # Get column names from all tables in the database
  columnNames <- getColumnNames(connection)
  # Filter out ID column names
  idColumnNames <- lapply(columnNames, function(x) grep("_id$", x, value = TRUE))
  # Exclude 'source' columns
  idColumnNames <- lapply(idColumnNames, function(x) grep("source", x, invert = TRUE, value = TRUE))
  return(idColumnNames)
}
