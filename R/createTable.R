#' @title Create OMOP CDM table
#' @description This function creates an Observational Medical Outcomes Partnership (OMOP) Common Data Model (CDM) table.
#' It performs several operations such as adding tables related to the 'person' table, reshaping them and resolving their ID values.
#' @param resource The resource to be used for creating the table.
#' @param removeConceptId A logical value indicating whether to remove columns with concept IDs. Default is TRUE.
#' @param removeNA A logical value indicating whether to remove columns with NA values. Default is FALSE.
#' @return A fully processed OMOP CDM table.
createTable <- function(resource, removeConceptId = TRUE, removeNA = FALSE) {
  # Establish a connection with the resource
  connection <- getConnection(resource)
  # Add tables to the 'person' table
  table <- addTables(connection)
  # Extend the table with additional information by resolving ID columns
  table <- extendTable(connection, table)
  # Sort the column names by their categories in the table
  table <- sortTableCategories(table)
  # Check if there is a "concept" table in the database before removing concept IDs
  if (!("concept" %in% getTableNames(connection))) {
    removeConceptId <- FALSE
  }
  # Clean the table by removing specified columns
  table <- cleanTable(table, removeConceptId = removeConceptId, removeNA = removeNA)
  return(table)
}
