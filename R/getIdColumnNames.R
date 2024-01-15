#' @export
getIdColumnNames <- function(connection) {
  columnNames <- getColumnNames(connection)
  idColumnNames <- lapply(columnNames, function(x) grep("_id$", x, value = TRUE))
  idColumnNames <- lapply(idColumnNames, function(x) grep("source", x, invert = TRUE, value = TRUE))
  return(idColumnNames)
}
