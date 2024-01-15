#' @export
getConnection <- function(resource) {
  if (!inherits(resource, "OMOPCDMResourceClient")) {
    stop("The provided resource is not an OMOP CDM database.")
  }
  connection <- resource$getConnection()
  return(connection)
}
