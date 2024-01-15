#' @title Extend a table with additional columns
#' @description This function extends a given table by adding additional columns. The columns are added based on the
#' matching of column names with the names in a dictionary of simplified concepts. The process continues
#' until either there are no more matching column names or a specified maximum depth is reached.
#' @param connection A DBI connection object to the database.
#' @param table The table to be extended.
#' @param maxDepth The maximum depth to which the table should be extended. Default is 5.
#' @return The extended table.
#' @details The function starts by retrieving a dictionary of simplified concepts and a list of non-related table ID
#' column names. It then enters a loop where it iterates over the column names. For each column name, it
#' checks if it matches any name in the dictionary. If a match is found, the corresponding column from the
#' dictionary is added to the table. The process continues until either there are no more matching column
#' names or the maximum depth is reached.
extendTable <- function(connection, table, maxDepth = 5) {
  # Get the dictionary of simplified concepts
  dictionary <- getSimplifiedConceptsDictionary(connection)
  # Get the non-related table ID column names
  columnNames <- getNonRelatedTableIdColumnNames(connection, table, "person")
  extendedColumns <- list()
  depth <- 0
  # Loop until there are no more column names or the maximum depth is reached
  while (length(columnNames) > 0 && depth < maxDepth) {
    for (columnName in columnNames) {
      for (name in names(dictionary)) {
        # If the column name matches a name in the dictionary, extend the table with the corresponding column
        if (grepl(name, columnName)) {
          dictionaryColumnName <- paste0(name, "_id")
          table <- extendColumn(table, dictionary[[name]], columnName, dictionaryColumnName)
          extendedColumns <- c(extendedColumns, columnName)
        }
      }
    }
    # Update the list of column names
    columnNames <- setdiff(getNonRelatedTableIdColumnNames(connection, table, "person"), extendedColumns)
    depth <- depth + 1
  }
  return(table)
}
