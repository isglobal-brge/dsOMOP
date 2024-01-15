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
