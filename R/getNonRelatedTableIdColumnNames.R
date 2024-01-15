#' @export
getNonRelatedTableIdColumnNames <- function(connection, table, nonRelatedTable) {
  idColumnNames <- getTableIdColumnNames(table)
  idCategories <- getNonRelatedIdCategories(connection, nonRelatedTable)
  nonRelatedIdColumnNames <- idColumnNames[sapply(idColumnNames, function(idColumnName) any(sapply(idCategories, function(idCategory) grepl(idCategory, idColumnName))))]
  return(nonRelatedIdColumnNames)
}
