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
    getConnection = function() {
      connection <- super$getConnection()
      if (is.null(connection)) {
        resource <- super$getResource()
        connection <- private$.dbi.connector$createDBIConnection(resource)
        super$setConnection(connection)
      }
      return(connection)
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
