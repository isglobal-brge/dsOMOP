#' @export
getIdsForColumn <- function(connection, columnName) {
  table <- strsplit(columnName, ".", fixed = TRUE)[[1]][1]
  column <- strsplit(columnName, ".", fixed = TRUE)[[1]][2]
  ids <- DBI::dbGetQuery(connection, paste0("SELECT DISTINCT ", column, " FROM ", table, " WHERE ", column, " IS NOT NULL"))
  return(ids[[1]])
}
