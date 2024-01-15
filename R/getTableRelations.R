#' @export
getTableRelations <- function(connection) {
  columnNames <- getClassifiedIdColumnNames(connection)
  names(columnNames) <- gsub("_id$", "", names(columnNames))
  columnNames <- lapply(columnNames, function(column) {
    column <- sapply(column, function(element) {
      strsplit(element, split = "\\.")[[1]][1]
    })
    unique(column)
  })
}
