#' @export
getIdCategories <- function(connection) {
  tableNames <- getTableNames(connection)
  idCategories <- paste0(tableNames, "_id")
  return(idCategories)
}
