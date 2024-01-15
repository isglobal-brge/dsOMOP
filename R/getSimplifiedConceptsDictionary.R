#' @export
getSimplifiedConceptsDictionary <- function(connection) {
  conceptDictionary <- getDictionary(connection)
  if ("concept" %in% names(conceptDictionary)) {
    conceptDictionary$concept <- conceptDictionary$concept[, c("concept_id", "concept_name")]
  }
  return(conceptDictionary)
}
