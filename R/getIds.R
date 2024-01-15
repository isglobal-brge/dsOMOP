#' @export
getIds <- function(connection) {
  idColumnNames <- getNonRelatedIdColumnNames(connection, "person")
  ids <- list()
  for (idCategory in names(idColumnNames)) {
    ids[[idCategory]] <- getIdsForCategory(connection, idColumnNames[[idCategory]])
  }
  return(ids)
}
