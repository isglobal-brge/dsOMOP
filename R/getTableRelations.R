#' @title Retrieve table relations from a database connection.
#' @description This function retrieves the table relations from a given database connection. It does this by first getting the classified ID column names from the database, then it removes the "_id" suffix from these names. After that, it splits each element of the column names by "." and keeps the first part of the split. Finally, it removes any duplicate names from each list of column names.
#' @param connection A DBI connection object to the database.
#' @return A list of unique column names from each table in the database.
getTableRelations <- function(connection) {
  # Get the classified ID column names from the database
  columnNames <- getClassifiedIdColumnNames(connection)
  # Remove the "_id" suffix from the column names
  names(columnNames) <- gsub("_id$", "", names(columnNames))
  # Split each element of the column names by "." and keep the first part of the split
  columnNames <- lapply(columnNames, function(column) {
    column <- sapply(column, function(element) {
      strsplit(element, split = "\\.")[[1]][1]
    })
    # Remove any duplicate names from each list of column names
    unique(column)
  })
}
