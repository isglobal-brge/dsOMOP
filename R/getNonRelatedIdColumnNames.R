#' @export
getNonRelatedIdColumnNames <- function(connection, tableName) {
  columnNames <- getClassifiedIdColumnNames(connection)
  idCategories <- getNonRelatedIdCategories(connection, tableName)
  columnNames <- columnNames[names(columnNames) %in% idCategories]
  return(columnNames)
}
