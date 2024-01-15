# Register the OMOP CDM resource resolver when the package is attached
.onAttach <- function(lib, pkg) {
  # Define a function to register a resource resolver
  registerResolver <- function(res) {
    # Get the class of the resolver
    class <- class(res)[[1]]
    # Display a startup message indicating the class being registered
    packageStartupMessage(paste0("Registering ", class, "..."))
    registerResourceResolver(res)
  }
  registerResolver(OMOPCDMResourceResolver$new())
}

# Unregister the OMOP CDM resource resolver when the package is detached
.onDetach <- function(lib) {
  unregisterResourceResolver("OMOPCDMResourceResolver")
}
