#' Get OMOP CDM Database Connection
#'
#' Retrieves an active database connection from the specified OMOP CDM resource client.
#' If no connection exists, it creates one using the DBI connector and sets it for future use.
#' This ensures that database operations are performed over a valid connection.
#' Additionally, it sets the search path to the schema specified in the resource, if present.
#'
#' @param resource An object of class OMOPCDMResourceClient representing the OMOP CDM resource client.
#'
#' @return A DBI database connection object.
#'
getConnection <- function(resource) {
  # Checks if the provided resource is an OMOP CDM resource client
  if (!inherits(resource, "OMOPCDMResourceClient")) {
    stop("The provided resource is not an OMOP CDM database.")
  }

  # Get the connection
  connection <- resource$getConnection()

  # Get the schema
  schema <- resource$getSchema()

  # If a schema is provided, set the schema
  if (!is.null(schema)) {
    # Get the DBMS
    dbms <- resource$getDBMS()

    # Get the schema query
    schemaQuery <- getSchemaQuery(dbms)

    # Set the schema
    DBI::dbExecute(connection, fillSchemaQuery(schema, schemaQuery))
  }
  
  return(connection)
}


#' Get Database Tables
#'
#' Retrieves a list of all tables available in the specified database connection and schema.
#'
#' @param connection A DBI database connection object.
#'
#' @return A character vector containing the names of all tables in the database.
#'
getTables <- function(connection) {
  tables <- DBI::dbListTables(connection)
  return(tables)
}


#' Get Table Columns
#'
#' Retrieves a list of all column names from the specified table within the database connection.
#' Optionally, it can exclude columns that are completely empty (NA values) by setting the `dropNA` parameter to TRUE.
#' When `dropNA` is TRUE, it first identifies empty columns and then excludes them from the list of returned columns.
#'
#' @param connection A DBI database connection object.
#' @param tableName The name of the table from which to retrieve column names.
#' @param dropNA A logical value indicating whether to exclude completely empty columns from the result.
#' @param caseInsensitive A logical value indicating whether to convert all column names to lowercase.
#'
#' @return A character vector containing the names of all columns in the table, optionally excluding empty ones.
#'
getColumns <- function(connection, tableName, dropNA = FALSE, caseInsensitive = TRUE) {
  # Checks if the specified table exists in the database
  if (!DBI::dbExistsTable(connection, tableName)) {
    stop(paste0("Table '", tableName, "' does not exist in the database."))
  }

  # Retrieves the list of columns from the specified table
  columns <- DBI::dbListFields(connection, tableName)

  # If the caseInsensitive flag is set, converts all column names to lowercase
  if (caseInsensitive) columns <- tolower(columns)

  # If the dropNA flag is set, removes columns with all NA values
  if (dropNA) {
    emptyColumns <- getEmptyColumns(connection, tableName)
    columns <- columns[!columns %in% emptyColumns]
  }

  return(columns)
}


#' Get Empty Columns
#'
#' Identifies and retrieves a list of column names from the specified table within the database connection
#' that are completely empty (i.e., all values are NA or empty strings). This function is useful for data
#' cleaning and preprocessing steps, allowing users to easily identify and exclude columns that contain no
#' useful information.
#'
#' @param connection A DBI database connection object.
#' @param tableName The name of the table to check for empty columns.
#'
#' @return A character vector containing the names of all completely empty columns in the table.
#'
getEmptyColumns <- function(connection, tableName) {
  emptyColumns <- vector("list") # Initializes an empty list to store the names of empty columns
  columns <- getColumns(connection, tableName)

  # Create a tbl object from the database connection and table name
  tableRef <- dplyr::tbl(connection, dplyr::sql(tableName))

  for (column in columns) {
    # Use dplyr to count non-empty values in the column
    columnCount <- tableRef %>%
      dplyr::summarize(count = sum(!is.na(!!dplyr::sym(column)) & as.character(!!dplyr::sym(column)) != "")) %>%
      dplyr::collect() %>%
      dplyr::pull(count)

    # If the column is empty, adds it to the list of empty columns
    if (columnCount == 0) {
      emptyColumns <- c(emptyColumns, column)
    }
  }

  return(emptyColumns)
}


#' Close Database Connection
#'
#' Closes an active database connection. If an error parameter is provided, the function will propagate the specified error.
#'
#' @param connection A DBI database connection object.
#' @param error An optional error message to be thrown if not NULL.
#'
closeConnection <- function(connection, error = NULL) {
  # Checks if the connection is valid and disconnects if necessary
  try({
    if (!is.null(connection)) {
      DBI::dbDisconnect(connection)
    }
  }, silent = TRUE)

  # If an error message is provided, throws the error
  if (!is.null(error)) {
    stop(error)
  }
}
