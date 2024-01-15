#' @export
getTable <- function(connection, tableName) {
  table <- DBI::dbReadTable(connection, tableName)
  table <- table[, !grepl("source", names(table))]
  return(table)
}
