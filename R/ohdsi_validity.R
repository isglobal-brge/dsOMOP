# Closed executable vocabulary-policy marker, shared by server and client.
.ohdsiValidityPolicy <- function(x = NULL) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || is.object(x) || length(x) != 1L || is.na(x) ||
      !identical(x, "ohdsi-import-1")) {
    stop("invalid_vocabulary_policy: use the installed ohdsi-import-1 policy or omit it for native semantics.",
         call. = FALSE)
  }
  x
}

# Walk executable definitions, distinguishing a registry's arbitrary user keys
# from fields of its entries. Only the envelope's advisory meta and a client
# population's Circe provenance are metadata locations. In particular, a
# registry entry called meta/metadata/circe_provenance is still executable.
# The same visitor drives capability discovery and server preflight so their
# definition inventories cannot diverge.
.ohdsiWalkValidity <- function(x, visit) {
  registries <- c("concept_sets", "events", "intervals", "populations",
                  "variables", "outputs", "blocks")
  walk <- function(node, context, path) {
    if (!is.list(node)) return(invisible(NULL))
    if (!context %in% c("registry", "population_registry") &&
        !is.null(node[["validity_policy"]]) &&
        any(c("concepts", "concept_id") %in% names(node))) {
      visit(node, path)
    }
    for (i in seq_along(node)) {
      key <- if (is.null(names(node))) "" else names(node)[[i]]
      if (identical(context, "root") && identical(key, "meta")) next
      if (identical(context, "population") && identical(key, "circe_provenance")) next
      child <- if (identical(context, "root") && key %in% registries) {
        if (identical(key, "populations")) "population_registry" else "registry"
      } else if (identical(context, "population_registry")) "population" else "node"
      walk(node[[i]], child, paste0(path, "/", i))
    }
    invisible(NULL)
  }
  walk(x, if (inherits(x, "omop_population")) "population" else "root", "")
  invisible(NULL)
}

.ohdsiHasImportedValidity <- function(x) {
  found <- FALSE
  .ohdsiWalkValidity(x, function(node, path) found <<- TRUE)
  found
}
