#' @title Retrieve foreign ID column names from database tables
#' @description This function retrieves the names of foreign ID columns from all tables in a database connection.
#' It returns a list where each element is a vector of foreign ID column names, excluding any columns named 'source'.
#' @param connection A DBI connection object to the database.
#' @return A list of character vectors. Each element of the list corresponds to a table in the database,
#' and contains the foreign ID column names of that table, excluding 'source' columns.
getForeignIdColumnNames <- function(connection) {
  # Retrieve ID column names from all tables in the database
  columnNames <- getIdColumnNames(connection)
  for (tableName in names(columnNames)) {
    # Exclude columns that are not foreign ID columns
    columnNames[[tableName]] <- columnNames[[tableName]][!grepl(paste0("^", tableName, "\\.", tableName, "_id$"), columnNames[[tableName]])]
  }
  return(columnNames)
}
