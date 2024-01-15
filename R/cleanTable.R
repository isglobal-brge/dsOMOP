#' @title Clean table
#' @description This function cleans a given table by optionally removing columns with concept IDs and/or columns with NA values.
#' @param table The input table to be cleaned.
#' @param removeConceptId A logical value indicating whether to remove columns with concept IDs. Default is TRUE.
#' @param removeNA A logical value indicating whether to remove columns with NA values. Default is FALSE.
#' @return A cleaned table.
cleanTable <- function(table, removeConceptId = TRUE, removeNA = FALSE) {
  # If removeConceptId is TRUE, remove columns with concept IDs
  if (removeConceptId) {
    table <- table[, !grepl("_concept_id", names(table))]
  }
  # If removeNA is TRUE, remove columns with NA values
  if (removeNA) {
    table <- table[, colSums(is.na(table)) != nrow(table)]
  }
  return(table)
}
