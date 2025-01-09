#' OMOP CDM Resource Client
#'
#' This R6 class manages connections and interactions with OMOP Common Data Model (CDM) databases.
#' It extends the generic ResourceClient class to provide OMOP CDM-specific functionality for database
#' connections, schema management, and resource handling.
#'
#' @description
#' The OMOPCDMResourceClient class provides methods to:
#' * Initialize connections with OMOP CDM databases
#' * Manage database connections lifecycle
#' * Extract and handle schema information
#' * Parse and utilize DBMS configuration
#'
#' @details
#' The class uses a URL-based configuration system with the format:
#' `dbms://host:port/database//dsomop::/schema:schema_name/vocabulary_schema:vocab_schema`
#'
#' Where:
#' * dbms: Database management system (e.g., postgresql, mysql)
#' * schema: Main database schema name
#' * vocabulary_schema: Schema containing OMOP vocabulary tables
#'
#' @examples
#' \dontrun{
#' # Create a resource object
#' resource <- list(
#'   url = "postgresql://localhost:5432/omop_db//dsomop::/schema:cdm/vocabulary_schema:vocab",
#'   format = "omop.cdm.db"
#' )
#'
#' # Initialize client
#' client <- OMOPCDMResourceClient$new(resource)
#'
#' # Get database connection
#' conn <- client$getConnection()
#'
#' # Close connection when done
#' client$close()
#' }
#'
#' @importFrom R6 R6Class
#' @export
OMOPCDMResourceClient <- R6::R6Class(
  "OMOPCDMResourceClient",
  inherit = ResourceClient,
  public = list(
    #' @description
    #' Initialize a new OMOPCDMResourceClient instance
    #'
    #' @param resource A list containing resource configuration (url, format)
    #' @param dbi.connector Optional DBI connector object. If NULL, attempts to find suitable connector
    #'
    #' @return A new OMOPCDMResourceClient object
    initialize = function(resource, dbi.connector = NULL) {
      # Initialize parent class
      super$initialize(resource)

      # Set up DBI connector - either use provided one or find suitable connector
      private$.dbi.connector <- if (is.null(dbi.connector)) {
        findDBIResourceConnector(resource)
      } else {
        dbi.connector
      }

      # Validate DBI connector availability
      if (is.null(private$.dbi.connector)) {
        stop("DBI resource connector cannot be found: either provide one or register one.")
      }
    },

    #' @description
    #' Get or create an active database connection
    #'
    #' @details
    #' This method manages the database connection lifecycle:
    #' 1. Checks for existing connection
    #' 2. If none exists, extracts base URL from resource
    #' 3. Creates new connection using DBI connector
    #' 4. Stores connection for reuse
    #'
    #' @return A DBI connection object
    getConnection = function() {
      connection <- super$getConnection()
      if (is.null(connection)) {
        resource <- super$getResource()
        # Extract base URL by removing dsomop configuration
        resourceUrl <- strsplit(resource$url, "//dsomop::")[[1]][1]
        resource$url <- resourceUrl
        # Create and store new connection
        connection <- private$.dbi.connector$createDBIConnection(resource)
        super$setConnection(connection)
      }
      return(connection)
    },

    #' @description
    #' Extract database schema name from resource URL
    #'
    #' @details
    #' Parses the resource URL to find schema configuration in format:
    #' `/schema:schema_name`
    #'
    #' @return Character string containing schema name, or NULL if not specified
    getSchema = function() {
      resource <- super$getResource()
      parts <- strsplit(resource$url, "//dsomop::")[[1]]
      if (length(parts) > 1) {
        config_part <- parts[2]
        schema_match <- regexec("/schema:([^/]+)", config_part)
        if (schema_match[[1]][1] != -1) {
          return(regmatches(config_part, schema_match)[[1]][2])
        }
      }
      return(NULL)
    },

    #' @description
    #' Extract vocabulary schema name from resource URL
    #'
    #' @details
    #' Parses the resource URL to find vocabulary schema configuration in format:
    #' `/vocabulary_schema:schema_name`
    #'
    #' @return Character string containing vocabulary schema name, or NULL if not specified
    getVocabularySchema = function() {
      resource <- super$getResource()
      parts <- strsplit(resource$url, "//dsomop::")[[1]]
      if (length(parts) > 1) {
        config_part <- parts[2]
        vocab_schema_match <- regexec("/vocabulary_schema:([^/]+)", config_part)
        if (vocab_schema_match[[1]][1] != -1) {
          return(regmatches(config_part, vocab_schema_match)[[1]][2])
        }
      }
      return(NULL)
    },

    #' @description
    #' Extract database management system type from resource URL
    #'
    #' @details
    #' Extracts DBMS identifier from start of URL (e.g., "postgresql", "mysql")
    #'
    #' @return Character string containing DBMS type
    getDBMS = function() {
      resource <- super$getResource()
      # Extract DBMS from URL prefix
      dbms <- sub("^(.*)://.*$", "\\1", resource$url)
      return(dbms)
    },

    #' @description
    #' Close active database connection
    #'
    #' @details
    #' Safely closes connection if one exists and resets connection state
    close = function() {
      connection <- super$getConnection()
      if (!is.null(connection)) {
        private$.dbi.connector$closeDBIConnection(connection)
        super$setConnection(NULL)
      }
    }
  ),
  private = list(
    .dbi.connector = NULL  # Stores DBI connector instance
  )
)

#' OMOP CDM Resource Resolver
#'
#' This R6 class handles resolution of OMOP CDM database resources. It validates
#' resource configurations and creates appropriate client instances for interacting
#' with OMOP CDM databases.
#'
#' @description
#' The resolver performs two main functions:
#' 1. Validates if a resource configuration is suitable for OMOP CDM
#' 2. Creates new OMOPCDMResourceClient instances for valid resources
#'
#' @examples
#' \dontrun{
#' resolver <- OMOPCDMResourceResolver$new()
#'
#' # Check if resource is suitable
#' resource <- list(
#'   url = "postgresql://localhost/omop",
#'   format = "omop.cdm.db"
#' )
#' is_suitable <- resolver$isFor(resource)
#'
#' # Create client if suitable
#' client <- resolver$newClient(resource)
#' }
#'
#' @export
OMOPCDMResourceResolver <- R6::R6Class(
  "OMOPCDMResourceResolver",
  inherit = ResourceResolver,
  public = list(
    #' @description
    #' Check if a resource is suitable for OMOP CDM handling
    #'
    #' @param resource Resource configuration to validate
    #' @return Logical indicating if resource is suitable
    isFor = function(resource) {
      isSuitable <- super$isFor(resource) &&
        !is.null(findDBIResourceConnector(resource)) &&
        tolower(resource$format) %in% c("omop.cdm.db")
      return(isSuitable)
    },

    #' @description
    #' Create new client for OMOP CDM resource
    #'
    #' @param resource Resource configuration
    #' @return New OMOPCDMResourceClient instance or NULL if resource unsuitable
    newClient = function(resource) {
      if (self$isFor(resource)) {
        client <- OMOPCDMResourceClient$new(resource)
        return(client)
      } else {
        return(NULL)
      }
    }
  )
)
