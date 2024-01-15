#' @export
createOMOPCDMTableDS <- function(resource, removeConceptId = TRUE, removeNA = FALSE) {
  table <- createTable(resource, removeConceptId = removeConceptId, removeNA = removeNA)
  return(table)
}
