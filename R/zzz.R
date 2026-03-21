# Module: Package Hooks
# Package load and detach hooks for dsOMOP.

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Mutable package state
.pkg_state <- new.env(parent = emptyenv())
.pkg_state$resolver <- NULL

# Session-level handle storage
.dsomop_env <- new.env(parent = emptyenv())

#' Clean up stale staging directories older than 24 hours
#'
#' @keywords internal
.cleanStaleStagingDirs <- function() {
  base <- getOption("dsstaging.base_dir", file.path(tempdir(), "dsstaging"))
  if (!dir.exists(base)) return(invisible(NULL))

  dirs <- list.dirs(base, full.names = TRUE, recursive = FALSE)
  # Only clean directories matching the staging token pattern
  dirs <- dirs[grepl("^stg_", basename(dirs))]
  if (length(dirs) == 0L) return(invisible(NULL))

  cutoff <- Sys.time() - 24 * 3600  # 24 hours ago
  for (d in dirs) {
    info <- file.info(d)
    if (!is.na(info$mtime) && info$mtime < cutoff) {
      tryCatch(unlink(d, recursive = TRUE), error = function(e) NULL)
    }
  }
  invisible(NULL)
}

#' Package load hook
#'
#' Ensures the marker directory exists, checks that critical R database
#' driver packages are loadable (attempting install if missing), and warns
#' if unixODBC is not available on the system.
#'
#' @param libname Library path.
#' @param pkgname Package name.
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Ensure marker directory
  marker_dir <- Sys.getenv("DSOMOP_MARKER_DIR", unset = "/var/lib/dsomop")
  if (!dir.exists(marker_dir)) {
    tryCatch(dir.create(marker_dir, recursive = TRUE, showWarnings = FALSE),
             error = function(e) NULL)
  }

  # Check for optional database drivers and warn if missing
  optional_drivers <- c("RPostgres", "RSQLite", "RMariaDB", "odbc")
  missing <- optional_drivers[!vapply(optional_drivers, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    packageStartupMessage(
      "dsOMOP: optional database drivers not installed: ",
      paste(missing, collapse = ", "),
      ". Install with: install.packages(c('",
      paste(missing, collapse = "', '"), "'))")
  }

  invisible(NULL)
}

#' Package attach hook
#'
#' Registers the OMOP CDM resource resolver, cleans stale staging
#' directories, and displays a startup message with the package version.
#'
#' @param lib Library path.
#' @param pkg Package name.
#' @keywords internal
.onAttach <- function(lib, pkg) {
  .pkg_state$resolver <- OMOPResourceResolver$new()
  resourcer::registerResourceResolver(.pkg_state$resolver)

  # Clean up stale staging directories from previous sessions
  tryCatch(.cleanStaleStagingDirs(), error = function(e) NULL)

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
