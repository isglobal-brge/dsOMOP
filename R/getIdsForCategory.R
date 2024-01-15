#' @export
getIdsForCategory <- function(connection, idCategory) {
  ids <- unique(unlist(lapply(idCategory, function(columnName) {
    getIdsForColumn(connection, columnName)
  })))
  return(ids)
}
