#' @title OMOP CDM resource resolver
#' @description A class that provides methods to resolve resources in the OMOP Common Data Model (CDM).
#' It inherits from the ResourceResolver class.
OMOPCDMResourceResolver <- R6::R6Class(
  "OMOPCDMResourceResolver",
  inherit = ResourceResolver,
  public = list(
    #' @description Checks if the resource is suitable for this resolver.
    #' @param resource The resource to be checked.
    #' @return A boolean indicating if the resource is suitable.
    isFor = function(resource) {
      # Check if the resource is suitable for the parent class, has a DBI connector, and is in the OMOP CDM format.
      isSuitable <- super$isFor(resource) &&
        !is.null(findDBIResourceConnector(resource)) &&
        tolower(resource$format) %in% c("omop.cdm.db")
      return(isSuitable)
    },
    #' @description Creates a new client for the resource if it is suitable.
    #' @param resource The resource for which a client is to be created.
    #' @return An instance of the OMOPCDMResourceClient class if the resource is suitable, NULL otherwise.
    newClient = function(resource) {
      if (self$isFor(resource)) {
        # Create a new client for the resource.
        client <- OMOPCDMResourceClient$new(resource)
        return(client)
      } else {
        return(NULL)
      }
    }
  )
)
