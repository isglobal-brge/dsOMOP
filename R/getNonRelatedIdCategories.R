#' @export
getNonRelatedIdCategories <- function(connection, tableName) {
  nonRelatedTables <- getNonRelatedTables(connection, tableName)
  nonClinicalDataCategories <- paste0(nonRelatedTables, "_id")
  return(nonClinicalDataCategories)
}
