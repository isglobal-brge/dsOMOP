#' @export
createTable <- function(resource, removeConceptId = TRUE, removeNA = FALSE) {
  connection <- getConnection(resource)
  table <- addTables(connection)
  table <- extendTable(connection, table)
  table <- sortTableCategories(table)
  table <- cleanTable(table, removeConceptId = removeConceptId, removeNA = removeNA)
  return(table)
}
