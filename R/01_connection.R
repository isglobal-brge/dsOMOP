#' @export
getConnection <- function(resource) {
  if (!inherits(resource, "OMOPCDMResourceClient")) {
    stop("The provided resource is not an OMOP CDM database.")
  }
  connection <- resource$getConnection()
  return(connection)
}

getTables <- function(connection) {
  tables <- DBI::dbListTables(connection)
  return(tables)
}

getColumns <- function(connection, tableName) {
  if (!DBI::dbExistsTable(connection, tableName)) {
    stop(paste0("Table '", tableName, "' does not exist in the database."))
  }
  columns <- DBI::dbListFields(connection, tableName)
  return(columns)
}
