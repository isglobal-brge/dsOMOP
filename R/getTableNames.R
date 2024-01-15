#' @export
getTableNames <- function(connection) {
  tables <- DBI::dbListTables(connection)
  return(tables)
}
