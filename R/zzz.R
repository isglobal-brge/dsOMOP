.onAttach <- function(lib, pkg) {
  registerResolver <- function(res) {
    class <- class(res)[[1]]
    packageStartupMessage(paste0("Registering ", class, "..."))
    registerResourceResolver(res)
  }
  registerResolver(OMOPCDMResourceResolver$new())
}

.onDetach <- function(lib) {
  unregisterResourceResolver("OMOPCDMResourceResolver")
}
