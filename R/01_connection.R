#' Get OMOP CDM Database Connection
#'
#' Retrieves an active database connection from the specified OMOP CDM resource client.
#' If no connection exists, it creates one using the DBI connector and sets it for future use.
#' This ensures that database operations are performed over a valid connection.
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

  connection <- resource$getConnection()
  return(connection)
}


#' Get Database Tables
#'
#' Retrieves a list of all tables available in the specified database connection.
#' This function utilizes the DBI package to list all tables present in the database
#' connected through the provided DBI connection object.
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
#'
#' @return A character vector containing the names of all columns in the table, optionally excluding empty ones.
#'
getColumns <- function(connection, tableName, dropNA = FALSE) {
  if (!DBI::dbExistsTable(connection, tableName)) {
    stop(paste0("Table '", tableName, "' does not exist in the database."))
  }

  # Retrieves the list of columns from the specified table
  columns <- DBI::dbListFields(connection, tableName)

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
  emptyColumns <- vector("list") # Initializes an empty list to store the names of empty columnss
  columns <- getColumns(connection, tableName)

  for (column in columns) {
    columnCount <- DBI::dbGetQuery(
      connection,

      # Count the number of non-empty values in the column
      paste0(
        "SELECT COUNT(", column, ") FROM ", tableName,
        " WHERE ", column, " IS NOT NULL AND",
        " CAST(", column, " AS TEXT) != ''"
      )
    )$count

    # If the column is empty, adds it to the list of empty columns
    if (columnCount == 0) {
      emptyColumns <- c(emptyColumns, column)
    }
  }

  return(emptyColumns)
}
