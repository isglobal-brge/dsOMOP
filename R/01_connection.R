#' Get OMOP CDM Database Connection
#'
#' Establishes and retrieves an active database connection from an OMOP CDM resource client.
#' This function handles connection management and schema configuration for OMOP CDM databases.
#'
#' @description
#' This function performs several key operations:
#' 1. Validates that the provided resource is a valid OMOP CDM resource client
#' 2. Retrieves or creates a database connection using the client
#' 3. Configures the database schema if specified
#' 4. Returns an active DBI connection object
#'
#' @param resource An object of class OMOPCDMResourceClient representing the OMOP CDM resource client.
#'                 Must be properly initialized with valid connection parameters.
#'
#' @return A DBI database connection object configured for the specified OMOP CDM database.
#'
#' @examples
#' \dontrun{
#' # Create a resource client
#' resource <- OMOPCDMResourceClient$new(list(
#'   url = "postgresql://localhost:5432/omop_db//dsomop::/schema:cdm",
#'   format = "omop.cdm.db"
#' ))
#'
#' # Get database connection
#' conn <- getConnection(resource)
#'
#' # Use connection for database operations
#' tables <- DBI::dbListTables(conn)
#'
#' # Close connection when done
#' closeConnection(conn)
#' }
#'
#' @seealso
#' * \code{\link{closeConnection}} for properly closing database connections
#' * \code{\link{OMOPCDMResourceClient}} for details on the resource client class
#'
getConnection <- function(resource) {
  # Step 1: Validate resource type
  if (!inherits(resource, "OMOPCDMResourceClient")) {
    stop("The provided resource is not an OMOP CDM database.")
  }

  # Step 2: Retrieve active connection from resource
  connection <- resource$getConnection()

  # Step 3: Configure schema if specified
  schema <- resource$getSchema()
  if (!is.null(schema)) {
    # Get DBMS-specific schema configuration
    dbms <- resource$getDBMS()
    schemaQuery <- getSchemaQuery(dbms)

    # Execute schema configuration query
    DBI::dbExecute(connection, fillSchemaQuery(schema, schemaQuery))
  }
  
  return(connection)
}

#' List Available Database Tables
#'
#' Retrieves a comprehensive list of all tables accessible in the connected database and schema.
#'
#' @description
#' This function provides a simple interface to list all available tables in the database.
#' It uses the DBI interface to ensure compatibility across different database management systems.
#'
#' @param connection A DBI database connection object. Must be an active connection.
#'
#' @return A character vector containing the names of all accessible tables in the database.
#'
#' @examples
#' \dontrun{
#' # Get connection
#' conn <- getConnection(resource)
#'
#' # List all tables
#' available_tables <- getTables(conn)
#' print(available_tables)
#'
#' # Close connection
#' closeConnection(conn)
#' }
#'
getTables <- function(connection) {
  tables <- DBI::dbListTables(connection)
  return(tables)
}

#' Get Table Column Information
#'
#' Retrieves and processes column information from a specified database table.
#'
#' @description
#' This function provides detailed column information with options for:
#' * Listing all columns in a table
#' * Excluding empty columns (containing only NA values)
#' * Case-insensitive column name handling
#'
#' @param connection A DBI database connection object
#' @param tableName Character string specifying the target table name
#' @param dropNA Logical; if TRUE, excludes columns containing only NA values (default: FALSE)
#' @param caseInsensitive Logical; if TRUE, converts all column names to lowercase (default: TRUE)
#'
#' @return A character vector containing the processed column names
#'
#' @examples
#' \dontrun{
#' conn <- getConnection(resource)
#'
#' # Get all columns
#' cols <- getColumns(conn, "person")
#'
#' # Get non-empty columns only
#' cols_no_empty <- getColumns(conn, "person", dropNA = TRUE)
#'
#' # Get case-sensitive column names
#' cols_case <- getColumns(conn, "person", caseInsensitive = FALSE)
#'
#' closeConnection(conn)
#' }
#'
getColumns <- function(connection, tableName, dropNA = FALSE, caseInsensitive = TRUE) {
  # Validate table existence
  if (!DBI::dbExistsTable(connection, tableName)) {
    stop(paste0("Table '", tableName, "' does not exist in the database."))
  }

  # Get column list
  columns <- DBI::dbListFields(connection, tableName)

  # Apply case conversion if requested
  if (caseInsensitive) {
    columns <- tolower(columns)
  }

  # Remove empty columns if requested
  if (dropNA) {
    emptyColumns <- getEmptyColumns(connection, tableName)
    columns <- columns[!columns %in% emptyColumns]
  }

  return(columns)
}

#' Identify Empty Database Columns
#'
#' Analyzes a database table to identify columns containing only NULL or empty values.
#'
#' @description
#' This function performs a comprehensive analysis of table columns to identify those that:
#' * Contain only NULL values
#' * Contain only empty strings
#' * Have no meaningful data
#'
#' The analysis is performed efficiently using SQL-level operations.
#'
#' @param connection A DBI database connection object
#' @param tableName Character string specifying the target table name
#'
#' @return Character vector containing names of empty columns
#'
#' @examples
#' \dontrun{
#' conn <- getConnection(resource)
#'
#' # Find empty columns in person table
#' empty_cols <- getEmptyColumns(conn, "person")
#' print(empty_cols)
#'
#' closeConnection(conn)
#' }
#'
getEmptyColumns <- function(connection, tableName) {
  # Initialize empty column collection
  emptyColumns <- vector("list")
  
  # Get all table columns
  columns <- getColumns(connection, tableName)

  # Create database table reference
  tableRef <- dplyr::tbl(connection, dplyr::sql(tableName))

  # Analyze each column
  for (column in columns) {
    # Count non-empty values using dplyr
    columnCount <- tableRef %>%
      dplyr::summarize(count = sum(!is.na(!!dplyr::sym(column)) & 
                                  as.character(!!dplyr::sym(column)) != "")) %>%
      dplyr::collect() %>%
      dplyr::pull(count)

    # Add to empty columns if no valid values found
    if (columnCount == 0) {
      emptyColumns <- c(emptyColumns, column)
    }
  }

  return(emptyColumns)
}

#' Close Database Connection Safely
#'
#' Ensures safe termination of database connections with optional error handling.
#'
#' @description
#' This function provides a safe way to:
#' * Close active database connections
#' * Handle any connection-related errors
#' * Propagate specific error messages if needed
#'
#' @param connection A DBI database connection object to be closed
#' @param error Optional error message to propagate after closing connection
#'
#' @examples
#' \dontrun{
#' # Simple connection close
#' closeConnection(conn)
#'
#' # Close with error propagation
#' closeConnection(conn, error = "Database operation failed")
#' }
#'
closeConnection <- function(connection, error = NULL) {
  # Attempt connection closure
  try({
    if (!is.null(connection)) {
      DBI::dbDisconnect(connection)
    }
  }, silent = TRUE)

  # Propagate error if specified
  if (!is.null(error)) {
    stop(error)
  }
}
