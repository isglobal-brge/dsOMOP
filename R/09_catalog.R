#' Retrieve Database Table Catalog
#'
#' @title Get Tables Catalog from Database
#' @description
#' Retrieves a catalog of all available tables within the connected database. This function
#' is designed to be called from the DataSHIELD client and handles database connections
#' safely with proper error handling and cleanup.
#'
#' @details
#' The function performs the following operations:
#' 1. Establishes database connection using provided resource
#' 2. Retrieves list of all tables in database
#' 3. Handles any errors during retrieval
#' 4. Ensures proper connection cleanup
#'
#' @param resource A database resource object containing connection details and credentials
#'
#' @return A character vector containing names of all tables in the database
#'
#' @examples
#' \dontrun{
#' # Get catalog of tables using database resource
#' resource <- DatabaseResource$new(
#'   driver = "PostgreSQL",
#'   dbname = "my_database"
#' )
#' tables <- getTableCatalogDS(resource)
#' }
#'
#' @export
#'
getTableCatalogDS <- function(resource) {
  # Step 1: Establish database connection
  connection <- getConnection(resource)

  # Step 2: Retrieve table catalog with error handling
  tryCatch(
    {
      tableCatalog <- getTables(connection)
    },
    error = function(error) {
      closeConnection(connection, error)
    }
  )

  # Step 3: Clean up and return results
  closeConnection(connection)
  return(tableCatalog)
}

#' Retrieve Column Catalog for Database Table
#'
#' @title Get Columns Catalog from Database Table
#' @description
#' Retrieves a catalog of all available columns from a specified database table. Provides
#' options for filtering empty columns and handles case-insensitive table name matching.
#'
#' @details
#' The function performs these key operations:
#' 1. Connection Management:
#'    * Establishes database connection
#'    * Ensures proper cleanup
#'
#' 2. Table Validation:
#'    * Performs case-insensitive table name matching
#'    * Validates table existence
#'
#' 3. Column Retrieval:
#'    * Gets all columns from specified table
#'    * Optionally filters empty columns
#'    * Handles retrieval errors gracefully
#'
#' @param resource A database resource object containing connection details
#' @param tableName Character string specifying target table name
#' @param dropNA Logical flag to exclude empty columns (default: FALSE)
#'
#' @return Character vector of column names from specified table
#'
#' @examples
#' \dontrun{
#' # Get all columns from person table
#' columns <- getColumnCatalogDS(
#'   resource = dbResource,
#'   tableName = "person"
#' )
#'
#' # Get non-empty columns from observation table
#' active_columns <- getColumnCatalogDS(
#'   resource = dbResource,
#'   tableName = "observation",
#'   dropNA = TRUE
#' )
#' }
#'
#' @export
#'
getColumnCatalogDS <- function(resource, tableName, dropNA = FALSE) {
  # Step 1: Establish database connection
  connection <- getConnection(resource)

  # Step 2: Retrieve columns with error handling
  tryCatch(
    {
      # Find case-insensitive table match
      tables <- getTables(connection)
      caseInsensitiveTableName <- findCaseInsensitiveTable(tables, tableName)
      
      # Validate table existence
      if (is.null(caseInsensitiveTableName)) {
        stop(paste0("The table '", tableName, "' does not exist in the database."))
      }
      tableName <- caseInsensitiveTableName

      # Get column list with optional NA filtering
      columnCatalog <- getColumns(connection, tableName, dropNA)
    },
    error = function(error) {
      closeConnection(connection, error)
    }
  )

  # Step 3: Clean up and return results
  DBI::dbDisconnect(connection)
  return(columnCatalog)
}

#' Retrieve Concept Catalog from Database Table
#'
#' @title Get Concept Catalog from OMOP CDM Table
#' @description
#' Retrieves a catalog of concepts from a specified OMOP CDM table, mapping concept IDs
#' to their corresponding concept names from the vocabulary tables. Handles schema
#' management, privacy filtering, and proper error handling.
#'
#' @details
#' The function performs these major operations:
#' 1. Connection & Schema Management:
#'    * Establishes database connection
#'    * Manages vocabulary and base schemas
#'    * Ensures proper schema restoration
#'
#' 2. Concept ID Retrieval:
#'    * Identifies relevant concept ID column
#'    * Applies privacy filters based on person count
#'    * Retrieves unique concept IDs
#'
#' 3. Concept Name Mapping:
#'    * Queries vocabulary tables for concept names
#'    * Handles missing concept mappings
#'    * Merges IDs with names preserving order
#'
#' Privacy Controls:
#' * Applies minimum person count filter for concepts
#' * Only retrieves concepts meeting privacy thresholds
#' * Handles sensitive data appropriately
#'
#' @param resource Database resource object with connection details
#' @param tableName Character string specifying the source table name
#'
#' @return Data frame containing:
#'   * concept_id: Numeric identifier for each concept
#'   * concept_name: Character string of concept names (NA if unmapped)
#'
#' @examples
#' \dontrun{
#' # Get concepts from condition table
#' condition_concepts <- getConceptCatalogDS(
#'   resource = dbResource,
#'   tableName = "condition_occurrence"
#' )
#'
#' # Get concepts from measurement table
#' measurement_concepts <- getConceptCatalogDS(
#'   resource = dbResource,
#'   tableName = "measurement"
#' )
#' }
#'
#' @export
#'
getConceptCatalogDS <- function(resource, tableName) {
  # Step 1: Initialize connection and schema information
  connection <- getConnection(resource)
  vocabularySchema <- resource$getVocabularySchema()
  dbms <- resource$getDBMS()
  subsetFilter <- getSubsetFilter()
  schemaQuery <- getSchemaQuery(dbms)
  schemaRetrievalQuery <- getSchemaRetrievalQuery(dbms)
  currentSchema <- DBI::dbGetQuery(connection, schemaRetrievalQuery)[[1]]
  schema <- currentSchema

  # Step 2: Retrieve concept catalog with error handling
  tryCatch(
    {
      # Validate table existence
      tables <- getTables(connection)
      caseInsensitiveTableName <- findCaseInsensitiveTable(tables, tableName)
      if (is.null(caseInsensitiveTableName)) {
        stop(paste0("The table '", tableName, "' does not exist in the database."))
      }
      tableName <- caseInsensitiveTableName

      # Switch to vocabulary schema if needed
      if (!is.null(vocabularySchema) && vocabularySchema != currentSchema) {
        DBI::dbExecute(connection, fillSchemaQuery(vocabularySchema, schemaQuery))
      }

      # Locate concept table
      tables <- getTables(connection)
      conceptTable <- findCaseInsensitiveTable(tables, "concept")

      # Restore base schema
      if (!is.null(schema)) {
        DBI::dbExecute(connection, fillSchemaQuery(schema, schemaQuery))
      }

      # Get and validate columns
      columns <- getColumns(connection, tableName, caseInsensitive = FALSE)
      conceptIdColumn <- findCaseInsensitiveColumn(columns, getConceptIdColumn(tableName))
      if (!conceptIdColumn %in% columns) {
        stop(paste0("The column '", conceptIdColumn, "' does not exist in the table '", tableName, "'."))
      }

      # Build and execute concept ID query with privacy filter
      query <- paste0("SELECT DISTINCT ", DBI::dbQuoteIdentifier(connection, conceptIdColumn), 
                     " FROM ", DBI::dbQuoteIdentifier(connection, tableName))
      if ("person_id" %in% columns) {
        query <- paste0(query, " WHERE ", conceptIdColumn, " IN (SELECT ", 
                       conceptIdColumn, " FROM ", DBI::dbQuoteIdentifier(connection, tableName),
                       " GROUP BY ", conceptIdColumn, 
                       " HAVING COUNT(DISTINCT person_id) >= ", subsetFilter, ")")
      }
      conceptIds <- DBI::dbGetQuery(connection, query)[[1]]

      # Retrieve concept names
      conceptCatalog <- tryCatch(
        {
          getConcepts(connection, conceptIds, conceptTable, dbms, schema, vocabularySchema)
        },
        error = function(error) {
          data.frame(concept_id = numeric(0), concept_name = character(0))
        }
      )

      # Merge and format results
      conceptIds <- data.frame(concept_id = conceptIds)
      conceptCatalog <- merge(conceptIds, conceptCatalog, by = "concept_id", all.x = TRUE)
      conceptCatalog$concept_id <- as.numeric(conceptCatalog$concept_id)
      conceptCatalog <- conceptCatalog[order(conceptCatalog$concept_id), ]
    },
    error = function(error) {
      closeConnection(connection, error)
    }
  )

  # Step 3: Clean up and return results
  closeConnection(connection)
  return(conceptCatalog)
}

#' Verify DataSHIELD Privacy Control Level
#'
#' @title Check DataSHIELD Privacy Settings
#' @description
#' Validates that the current DataSHIELD privacy control level is set to either
#' 'permissive' or 'banana', which are required for package operations. Returns
#' any error messages if privacy settings are insufficient.
#'
#' @details
#' Privacy Levels:
#' * permissive: Standard permissive mode
#' * banana: Alternative permissive mode
#' 
#' The function checks these levels to ensure:
#' * Proper data access permissions
#' * Required operation capabilities
#' * Compliance with DataSHIELD security model
#'
#' @return Character string containing error message if privacy level is insufficient,
#'         NULL otherwise
#'
#' @examples
#' \dontrun{
#' # Check privacy settings
#' error <- checkPrivacyControlLevelDS()
#' if (!is.null(error)) {
#'   print(paste("Privacy check failed:", error))
#' }
#' }
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

#' Verify Database Connection Status
#'
#' @title Test Database Connection
#' @description
#' Validates that a database connection can be successfully established and closed
#' using the provided resource object. Serves as a connection test utility for
#' DataSHIELD clients.
#'
#' @details
#' The function performs these checks:
#' 1. Connection establishment
#' 2. Proper connection closure
#' 3. Error handling for connection issues
#'
#' @param resource Database resource object containing connection parameters
#'
#' @examples
#' \dontrun{
#' # Test connection with database resource
#' resource <- DatabaseResource$new(
#'   driver = "PostgreSQL",
#'   dbname = "test_db"
#' )
#' checkConnectionDS(resource)
#' }
#'
#' @export
#'
checkConnectionDS <- function(resource) {
  connection <- getConnection(resource)
  closeConnection(connection)
}