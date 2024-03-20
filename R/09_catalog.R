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
  # Opens a connection to the database
  connection <- getConnection(resource)

  # Attempts to retrieve the list of tables from the database
  tryCatch(
    {
      tableCatalog <- getTables(connection)
    },
    # In case of an error, closes the database connection and propagates the error
    error = function(error) {
      closeConnection(connection, error)
    }
  )

  # If the retrieval was successful, closes the database connection and returns the table catalog
  closeConnection(connection)
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
  # Opens a connection to the database
  connection <- getConnection(resource)

  # Attempts to retrieve the list of columns from the specified table
  tryCatch(
    {
      # Finds the case-insensitive table name to ensure correct retrieval
      tables <- getTables(connection)
      caseInsensitiveTableName <- findCaseInsensitiveTable(tables, tableName)
      if (is.null(caseInsensitiveTableName)) {
        stop(paste0("The table '", tableName, "' does not exist in the database."))
      }
      tableName <- caseInsensitiveTableName

      # Retrieves the list of columns from the specified table
      columnCatalog <- getColumns(connection, tableName, dropNA)
    },
    # In case of an error, closes the database connection and propagates the error
    error = function(error) {
      closeConnection(connection, error)
    }
  )

  # If the retrieval was successful, closes the database connection and returns the column catalog
  DBI::dbDisconnect(connection)
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
  # Opens a connection to the database
  connection <- getConnection(resource)

  # Attempts to retrieve the concept catalog from the specified table
  tryCatch(
    {
      # Finds the case-insensitive table name to ensure correct retrieval
      tables <- getTables(connection)
      caseInsensitiveTableName <- findCaseInsensitiveTable(tables, tableName)
      if (is.null(caseInsensitiveTableName)) {
        stop(paste0("The table '", tableName, "' does not exist in the database."))
      }
      tableName <- caseInsensitiveTableName

      # Attempts to identify the 'concept' table in the database
      tables <- getTables(connection)
      conceptTable <- findCaseInsensitiveTable(tables, "concept")

      # Gets the required column names for this operation
      columns <- getColumns(connection, tableName, caseInsensitive = FALSE)
      conceptIdColumn <- findCaseInsensitiveColumn(columns, getConceptIdColumn(tableName))

      # Checks if the concept ID column exists in the table
      if (!conceptIdColumn %in% columns) {
        stop(paste0("The column '", conceptIdColumn, "' does not exist in the table '", tableName, "'."))
      }

      # Retrieves the unique concept IDs from the table
      conceptIds <- DBI::dbGetQuery(connection, paste0("SELECT DISTINCT \"", conceptIdColumn, "\" FROM \"", tableName, "\""))
      conceptIds <- conceptIds[[1]]

      # Retrieves the concepts from the 'concept' table
      conceptCatalog <- tryCatch(
        {
          getConcepts(connection, conceptIds, conceptTable)
        },
        # In case of an error, returns an empty data frame (with the same structure as the expected output)
        error = function(error) {
          data.frame(concept_id = numeric(0), concept_name = character(0))
        }
      )

      # Merges the concept IDs with the concept names
      # This is done to ensure that even concept IDs that are not present in the 'concept' table are included
      # It does not involve sensitive data, so there is no need to use DataSHIELD's mergeDS function
      conceptIds <- data.frame(concept_id = conceptIds)
      conceptCatalog <- merge(conceptIds, conceptCatalog, by = "concept_id", all.x = TRUE)
    },
    # In case of an error, closes the database connection and propagates the error
    error = function(error) {
      closeConnection(connection, error)
    }
  )

  # If the retrieval was successful, closes the database connection and returns the concept catalog
  closeConnection(connection)
  return(conceptCatalog)
}


#' Check Privacy Control Level
#'
#' This function checks if the current privacy control level is set to either 'permissive' or 'banana'.
#' It is designed to be called from the DataSHIELD client to verify that the privacy control level is permissive enough to allow the
#' required operations used by the package.
#' If the privacy control level is not set to 'permissive' or 'banana', the resulting error is returned.
#'
#' @return An error message if the privacy control level is not set to 'permissive' or 'banana'.
#'
#' @export
#'
checkPrivacyControlLevelDS <- function() {
  tryCatch(
    {
      dsBase::checkPermissivePrivacyControlLevel(c("permissive", "banana"))
    },
    error = function(error) {
      return(error$message)
    }
  )
}


#' Check Database Connection
#'
#' This function checks if a connection to the database can be established using the provided resource.
#' It is designed to be called from the DataSHIELD client to verify that the database connection is active and valid.
#'
#' @param resource An object representing the database resource.
#'
#' @export
#'
checkConnectionDS <- function(resource) {
  connection <- getConnection(resource)
  closeConnection(connection)
}
