#' OMOP CDM Resource Client
#'
#' This class is designed to manage connections and interactions with OMOP Common Data Model (CDM) resources.
#' It extends the functionality of the generic ResourceClient class to specifically support OMOP CDM databases.
#' The class provides methods to initialize the resource client with a specific DBI connector if provided,
#' otherwise, it attempts to find a suitable connector. It also manages database connections, ensuring
#' that a connection is established when needed and properly closed when no longer in use.
#'
#' @field initialize Constructor method to initialize the OMOPCDMResourceClient object with a specific resource
#' and an optional DBI connector. If no DBI connector is provided, it attempts to find one. It throws an error
#' if no suitable DBI connector can be found.
#'
#' @field getConnection Retrieves an active database connection. If no connection exists, it creates one using
#' the DBI connector and sets it for future use. This ensures that database operations are performed over
#' a valid connection.
#'
#' @field close Closes the active database connection if it exists and sets the connection to NULL. This method
#' ensures that resources are properly released when the connection is no longer needed.
#'
OMOPCDMResourceClient <- R6::R6Class(
  "OMOPCDMResourceClient",
  inherit = ResourceClient,
  public = list(
    initialize = function(resource, dbi.connector = NULL) {
      super$initialize(resource)
      private$.dbi.connector <- if (is.null(dbi.connector)) {
        findDBIResourceConnector(resource)
      } else {
        dbi.connector
      }
      if (is.null(private$.dbi.connector)) {
        stop("DBI resource connector cannot be found: either provide one or register one.")
      }
    },
    
    #' @description
    #' Retrieves an active database connection. If no connection exists, it creates one using
    #' the DBI connector and sets it for future use. This ensures that database operations are
    #' performed over a valid connection.
    #' @return A DBI connection object.
    getConnection = function() {
      connection <- super$getConnection()
      if (is.null(connection)) {
        resource <- super$getResource()
        # Exclude the schema part if it exists
        resourceUrl <- sub("\\?schema=.*$", "", resource$url)
        resource$url <- resourceUrl
        connection <- private$.dbi.connector$createDBIConnection(resource)
        super$setConnection(connection)
      }
      return(connection)
    },
    
    #' @description
    #' Extracts the schema part from the resource URL if it exists.
    #' @return The schema as a string, or NULL if no schema is specified.
    getSchema = function() {
      resource <- super$getResource()
      # Extract the schema part if it exists
      schema <- sub(".*\\?schema=(.*)$", "\\1", resource$url)
      if (schema == resource$url) {
        schema <- NULL
      }
      return(schema)
    },
    
    #' @description
    #' Extracts the DBMS part from the resource URL.
    #' @return The DBMS as a string.
    getDBMS = function() {
      resource <- super$getResource()
      # Extract the DBMS part from the resource URL
      dbms <- sub("^(.*)://.*$", "\\1", resource$url)
      return(dbms)
    },

    close = function() {
      connection <- super$getConnection()
      if (!is.null(connection)) {
        private$.dbi.connector$closeDBIConnection(connection)
        super$setConnection(NULL)
      }
    }
  ),
  private = list(
    .dbi.connector = NULL
  )
)


#' OMOP CDM Resource Resolver
#'
#' This class is responsible for resolving resources specific to the OMOP Common Data Model (CDM).
#' It inherits from the ResourceResolver class and implements methods to determine if a resource
#' is suitable for OMOP CDM and to create a new client for interacting with OMOP CDM resources.
#'
#' @field isFor Checks if the given resource is suitable for OMOP CDM based on specific criteria.
#' @field newClient Creates a new OMOPCDMResourceClient for the given resource if it is suitable.
#'
OMOPCDMResourceResolver <- R6::R6Class(
  "OMOPCDMResourceResolver",
  inherit = ResourceResolver,
  public = list(
    isFor = function(resource) {
      isSuitable <- super$isFor(resource) &&
        !is.null(findDBIResourceConnector(resource)) &&
        tolower(resource$format) %in% c("omop.cdm.db")
      return(isSuitable)
    },
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
