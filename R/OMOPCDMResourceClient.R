#' @title OMOP CDM resource client
#' @description A class that provides methods to interact with an OMOP CDM database resource.
#' It inherits from the ResourceClient class.
OMOPCDMResourceClient <- R6::R6Class(
  "OMOPCDMResourceClient",
  inherit = ResourceClient,
  public = list(
    #' @description Initialize the OMOPCDMResourceClient object.
    #' @param resource The resource to be connected.
    #' @param dbi.connector (optional) The DBI connector to be used. If not provided, it will try to find one.
    #' @return An instance of the OMOPCDMResourceClient class.
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
    #' @description Get the connection to the resource.
    #' @return The connection to the resource.
    getConnection = function() {
      connection <- super$getConnection()
      if (is.null(connection)) {
        resource <- super$getResource()
        connection <- private$.dbi.connector$createDBIConnection(resource)
        super$setConnection(connection)
      }
      return(connection)
    },
    #' @description Close the connection to the resource.
    close = function() {
      connection <- super$getConnection()
      if (!is.null(connection)) {
        private$.dbi.connector$closeDBIConnection(connection)
        super$setConnection(NULL)
      }
    }
  ),
  private = list(
    .dbi.connector = NULL # The DBI connector used to interact with the resource.
  )
)
