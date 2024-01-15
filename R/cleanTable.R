#' @export
cleanTable <- function(table, removeConceptId = TRUE, removeNA = FALSE) {
  if (removeConceptId) {
    table <- table[, !grepl("_concept_id", names(table))]
  }
  if (removeNA) {
    table <- table[, colSums(is.na(table)) != nrow(table)]
  }
  return(table)
}
