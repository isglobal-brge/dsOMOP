#' @export
getClassifiedIdColumnNames <- function(connection) {
  idCategories <- getIdCategories(connection)
  idColumnNames <- getForeignIdColumnNames(connection)
  classifiedIdColumnNames <- list()
  for (tableName in names(idColumnNames)) {
    for (idColumnName in idColumnNames[[tableName]]) {
      idCategory <- matchIdCategory(idColumnName, idCategories)
      if (!is.null(idCategory)) {
        classifiedIdColumnNames[[idCategory]] <- c(classifiedIdColumnNames[[idCategory]], idColumnName)
      }
    }
  }
  return(classifiedIdColumnNames)
}
