# ==============================================================================
# dsOMOP v2 - Package Lifecycle
# ==============================================================================

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Mutable package state
.pkg_state <- new.env(parent = emptyenv())
.pkg_state$resolver <- NULL

# Session-level handle storage
.dsomop_env <- new.env(parent = emptyenv())

.onAttach <- function(lib, pkg) {
  .pkg_state$resolver <- OMOPResourceResolver$new()
  resourcer::registerResourceResolver(.pkg_state$resolver)

  packageStartupMessage(
    "dsOMOP v", utils::packageVersion("dsOMOP"),
    " loaded. OMOP CDM resource resolver registered."
  )
}

.onDetach <- function(lib) {
  if (!is.null(.pkg_state$resolver)) {
    tryCatch(
      resourcer::unregisterResourceResolver(.pkg_state$resolver),
      error = function(e) NULL
    )
    .pkg_state$resolver <- NULL
  }
}
