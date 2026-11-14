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

.ohdsiHasImportedValidity <- function(x) {
  if (!is.list(x)) return(FALSE)
  if (!is.null(x[["validity_policy"]]) && "concepts" %in% names(x)) return(TRUE)
  if (!is.null(names(x))) x <- x[!names(x) %in% c("meta", "metadata", "circe_provenance")]
  any(vapply(x, .ohdsiHasImportedValidity, logical(1)))
}
