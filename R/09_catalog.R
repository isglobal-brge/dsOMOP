#' @export
getTableCatalogDS <- function(resource) {
  connection <- getConnection(resource)
  tables <- getTables(connection)
  return(tableCatalog)
}

#' @export
getColumnCatalogDS <- function(resource, tableName) {
  connection <- getConnection(resource)
  columnCatalog <- getColumns(connection, tableName)
  return(columnCatalog)
}

#' @export
getConceptCatalogDS <- function(resource, tableName) {
  connection <- getConnection(resource)
  columns <- getColumns(connection, tableName)
  conceptIdColumn <- getConceptIdColumn(tableName)

  # Checks if the concept ID column exists in the table
  if (!conceptIdColumn %in% columns) {
    stop(paste0("The column '", conceptIdColumn, "' does not exist in the table '", tableName, "'."))
  }

  # Retrieves the unique concept IDs from the table
  conceptIds <- DBI::dbGetQuery(connection, paste0("SELECT DISTINCT ", conceptIdColumn, " FROM ", tableName))
  conceptIds <- conceptIds[[1]]

  # Retrieves the concepts from the 'concept' table
  conceptCatalog <- getConcepts(connection, conceptIds)

  # Merges the concept IDs with the concept names
  # This is done to ensure that even concept IDs that are not present in the 'concept' table are included
  conceptIds <- data.frame(concept_id = conceptIds)
  conceptCatalog <- merge(conceptIds, conceptCatalog, by = "concept_id", all.x = TRUE)
  
  return(conceptCatalog)
}
