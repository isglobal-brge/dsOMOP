#' @export
getForeignIdColumnNames <- function(connection) {
  columnNames <- getIdColumnNames(connection)
  for (tableName in names(columnNames)) {
    columnNames[[tableName]] <- columnNames[[tableName]][!grepl(paste0("^", tableName, "\\.", tableName, "_id$"), columnNames[[tableName]])]
  }
  return(columnNames)
}
