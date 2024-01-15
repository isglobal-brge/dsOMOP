#' @export
getNextRelation <- function(connection, tableName, targetTableName) {
  relationPath <- getRelationPath(connection, tableName, targetTableName)
  return(relationPath[1])
}
