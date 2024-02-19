#' Get Tables Catalog
#'
#' This function is called from the DataSHIELD client to fetch a catalog of all available tables within the database.
#'
#' @param connection A DBI database connection object.
#'
#' @return A character vector containing the names of all tables in the database.
#'
#' @export
#'
getTableCatalogDS <- function(resource) {
  connection <- getConnection(resource)
  tableCatalog <- getTables(connection)
  return(tableCatalog)
}


#' Get Columns Catalog
#'
#' This function is called from the DataSHIELD client to fetch a catalog of all available columns from a specific table within
#' the database. It allows for an optional filtering to exclude columns that are empty.
#'
#' @param resource A resource object representing the database connection.
#' @param tableName The name of the table from which to retrieve the column names.
#' @param dropNA A logical flag indicating whether to exclude columns that are completely empty (default is FALSE).
#'
#' @return A character vector containing the names of all columns in the specified table, optionally excluding empty ones.
#'
#' @export
#'
getColumnCatalogDS <- function(resource, tableName, dropNA = FALSE) {
  connection <- getConnection(resource)
  columnCatalog <- getColumns(connection, tableName, dropNA)
  return(columnCatalog)
}


#' Get Concept Catalog
#'
#' This function is called from the DataSHIELD client to fetch a catalog of available concept types from a specific table within
#' the database. It retrieves the unique concept IDs from the specified table and then fetches the corresponding concept types
#' from the 'concept' table.
#'
#' @param resource A resource object representing the database connection.
#' @param tableName The name of the table from which to retrieve the concept types.
#'
#' @return A data frame containing the unique concept IDs and their corresponding concept types from the specified table.
#'
#' @export
#'
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
