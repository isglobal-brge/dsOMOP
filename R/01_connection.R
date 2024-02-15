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

getColumns <- function(connection, tableName, dropNA = FALSE) {
  if (!DBI::dbExistsTable(connection, tableName)) {
    stop(paste0("Table '", tableName, "' does not exist in the database."))
  }
  columns <- DBI::dbListFields(connection, tableName)

  if (dropNA) {
    emptyColumns <- getEmptyColumns(connection, tableName)
    columns <- columns[!columns %in% emptyColumns]
  }

  return(columns)
}

getEmptyColumns <- function(connection, tableName) {
  emptyColumns <- vector("list")
  columns <- getColumns(connection, tableName)
  for (column in columns) {
    columnCount <- DBI::dbGetQuery(connection, 
                                   paste0("SELECT COUNT(", column, ") FROM ", tableName, 
                                          " WHERE ", column, " IS NOT NULL AND", 
                                          " CAST(", column, " AS TEXT) != ''"))$count
    if (columnCount == 0) {
      emptyColumns <- c(emptyColumns, column)
    }
  }
  return(emptyColumns)
}