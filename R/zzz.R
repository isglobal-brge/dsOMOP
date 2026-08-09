# Module: Package Hooks
# Package load and detach hooks for dsOMOP.

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Mutable package state
.pkg_state <- new.env(parent = emptyenv())
.pkg_state$resolver <- NULL
.pkg_state$dp_status <- NULL
.pkg_state$dp_runtime <- NULL
.pkg_state$dp_bootstrap_in_progress <- FALSE

# Session-level handle storage
.dsomop_env <- new.env(parent = emptyenv())

#' Clean up stale staging directories older than 24 hours
#'
#' @keywords internal
.cleanStaleStagingDirs <- function() {
  base <- .stagingBaseDir()

  dirs <- list.dirs(base, full.names = TRUE, recursive = FALSE)
  # Only clean directories matching the staging token pattern
  dirs <- dirs[grepl("^stg_[0-9a-f]{32}$", basename(dirs))]
  if (length(dirs) == 0L) return(invisible(NULL))

  ttl_hours <- suppressWarnings(as.numeric(getOption("dsstaging.ttl_hours", 24)))
  if (length(ttl_hours) != 1L || is.na(ttl_hours) || !is.finite(ttl_hours) ||
      ttl_hours <= 0) {
    stop("dsstaging.ttl_hours must be one positive finite number.", call. = FALSE)
  }
  cutoff <- Sys.time() - ttl_hours * 3600
  for (d in dirs) {
    if (.isSymbolicLink(d) ||
        !identical(normalizePath(dirname(d), winslash = "/", mustWork = TRUE),
                   base)) {
      next
    }
    info <- file.info(d)
    if (!is.na(info$mtime) && info$mtime < cutoff) {
      tryCatch(unlink(d, recursive = TRUE), error = function(e) NULL)
    }
  }
  invisible(NULL)
}

#' Package load hook
#'
#' Validates public pseudonym lifecycle configuration in real service runtimes
#' and registers the resource resolver.
#'
#' @param libname Library path.
#' @param pkgname Package name.
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # The sticky identity is a versioned wire contract. Detect accidental
  # canonicalizer drift on every namespace load, including package checks.
  .dsomopDpCanonicalSelfTest()

  # Never generate or read key material while loading a namespace. Armadillo
  # also applies its authoritative DataSHIELD profile options only after
  # loading packages, so binding privacy configuration here would be both too
  # early and backend-dependent. The first real dsOMOP service entry point
  # validates the final options and atomically initializes durable state.
  if (!.dsomopIsInstallOrDevelopmentLoad(libname)) {
    .dsomopPseudonymLifecycleSettings()
  }
  .pkg_state$dp_runtime <- NULL
  .pkg_state$dp_bootstrap_in_progress <- FALSE
  .pkg_state$dp_status <- .dsomopDpDormantStatus()

  # Opal commonly loads the namespace without attaching the package.
  tryCatch(.cleanStaleStagingDirs(), error = function(e) NULL)

  # Register the resource resolver on namespace LOAD, not attach: Opal invokes
  # methods as `dsOMOP::fn`, which loads the namespace without attaching, so
  # .onAttach never runs server-side — yet the resolver must already exist when
  # a resource is assigned (which happens before any dsOMOP method is called).
  .pkg_state$resolver <- OMOPResourceResolver$new()
  resourcer::registerResourceResolver(.pkg_state$resolver)

  invisible(NULL)
}

#' Package attach hook
#'
#' Cleans stale staging directories and displays a startup message with the
#' package version. (The resource resolver is registered in \code{.onLoad} so
#' it is available in server-side sessions that load the namespace without
#' attaching the package.)
#'
#' @param lib Library path.
#' @param pkg Package name.
#' @keywords internal
.onAttach <- function(lib, pkg) {
  # Clean up stale staging directories from previous sessions
  tryCatch(.cleanStaleStagingDirs(), error = function(e) NULL)

  optional_drivers <- c("RPostgres", "RSQLite", "RMariaDB", "odbc")
  missing <- optional_drivers[
    !vapply(optional_drivers, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) > 0L) {
    packageStartupMessage(
      "dsOMOP: optional database drivers not installed: ",
      paste(missing, collapse = ", "),
      ". Install with: install.packages(c('",
      paste(missing, collapse = "', '"), "'))"
    )
  }

  packageStartupMessage(
    "dsOMOP v", utils::packageVersion("dsOMOP"),
    " loaded. OMOP CDM resource resolver registered."
  )
}

.unregisterOMOPResourceResolver <- function() {
  if (!is.null(.pkg_state$resolver)) {
    # resourcer's unregister API takes a class name, not the resolver instance
    # accepted by registerResourceResolver(). Do not forget local ownership if
    # registry cleanup fails: a stale resolver must never be hidden.
    resourcer::unregisterResourceResolver("OMOPResourceResolver")
    remaining <- any(vapply(
      resourcer::getResourceResolvers(), inherits, logical(1),
      "OMOPResourceResolver"
    ))
    if (remaining) {
      stop("OMOP resource resolver could not be unregistered.", call. = FALSE)
    }
    .pkg_state$resolver <- NULL
  }
  invisible(NULL)
}

# Namespace unloading does not invoke .onDetach(). Release the resolver from
# the resourcer registry while this namespace and its R6 class are still live.
.onUnload <- function(libpath) {
  .unregisterOMOPResourceResolver()
}

# Keep the detach hook for explicit package detachment in interactive sessions.
.onDetach <- function(libpath) {
  .unregisterOMOPResourceResolver()
}
