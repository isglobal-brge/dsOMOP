#' @export
getTableIdColumnNames <- function(table) {
  columnNames <- colnames(table)
  idColumnNames <- grep("_id", columnNames, value = TRUE)
  return(idColumnNames)
}
