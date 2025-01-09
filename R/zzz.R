#' Package Initialization and Cleanup Functions
#'
#' @description
#' These functions handle the initialization and cleanup of the package when it is
#' loaded into or unloaded from an R session. They manage the registration and
#' unregistration of the OMOP CDM resource resolver.
#'
#' @details
#' The package uses two special R functions:
#' * .onAttach: Called when package is attached to R session
#' * .onDetach: Called when package is detached from R session
#'
#' The .onAttach function performs these steps:
#' 1. Defines an inner helper function to register resolvers
#' 2. Creates and registers the OMOP CDM resource resolver
#' 3. Displays registration status message to user
#'
#' The .onDetach function:
#' 1. Cleans up by unregistering the OMOP CDM resolver
#' 2. Ensures proper resource cleanup on package unload
#'
#' @param lib The library where the package is installed
#' @param pkg The name of the package being loaded
#'
#' @keywords internal
#' @noRd

.onAttach <- function(lib, pkg) {
  # Helper function to register resource resolvers
  registerResolver <- function(res) {
    # Get class name of resolver for status message
    class <- class(res)[[1]]
    
    # Display registration status message
    packageStartupMessage(paste0("Registering ", class, "..."))
    
    # Register the resolver
    registerResourceResolver(res)
  }
  
  # Create and register OMOP CDM resolver on package attach
  registerResolver(OMOPCDMResourceResolver$new())
}

#' @param lib The library where the package is installed
#' @keywords internal
#' @noRd

.onDetach <- function(lib) {
  # Clean up by unregistering the OMOP CDM resolver
  unregisterResourceResolver("OMOPCDMResourceResolver")
}
