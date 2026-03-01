# Module: Package Hooks
# Package load and detach hooks for dsOMOP.

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Mutable package state
.pkg_state <- new.env(parent = emptyenv())
.pkg_state$resolver <- NULL

# Session-level handle storage
.dsomop_env <- new.env(parent = emptyenv())

#' Package attach hook
#'
#' Registers the OMOP CDM resource resolver and displays a startup message
#' with the package version.
#'
#' @param lib Library path.
#' @param pkg Package name.
#' @keywords internal
.onAttach <- function(lib, pkg) {
  .pkg_state$resolver <- OMOPResourceResolver$new()
  resourcer::registerResourceResolver(.pkg_state$resolver)

  packageStartupMessage(
    "dsOMOP v", utils::packageVersion("dsOMOP"),
    " loaded. OMOP CDM resource resolver registered."
  )
}

#' Package detach hook
#'
#' Unregisters the OMOP CDM resource resolver on package unload.
#'
#' @param lib Library path.
#' @keywords internal
.onDetach <- function(lib) {
  if (!is.null(.pkg_state$resolver)) {
    tryCatch(
      resourcer::unregisterResourceResolver(.pkg_state$resolver),
      error = function(e) NULL
    )
    .pkg_state$resolver <- NULL
  }
}
