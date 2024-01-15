#' @title Get connection from a resource
#' @description This function retrieves the connection from an OMOP CDM database resource.
#' @param resource An object of class 'OMOPCDMResourceClient' representing the OMOP CDM database resource.
#' @return A connection object to the OMOP CDM database.
getConnection <- function(resource) {
  # Check if the resource is of the correct class
  if (!inherits(resource, "OMOPCDMResourceClient")) {
    stop("The provided resource is not an OMOP CDM database.")
  }
  # Retrieve the connection from the resource
  connection <- resource$getConnection()
  return(connection)
}
