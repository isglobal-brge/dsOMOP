#' Convert Concept ID Columns to Factors in the Table
#'
#' This function takes a data frame and converts all columns that contain "concept_id" in their name to factors.
#'
#' @param table A data frame with one or more columns that contain "concept_id" in their name.
#'
#' @return A data frame with columns that contain "concept_id" in their name converted to factors.
#'
conceptsToFactors <- function(table) {
  for (column in names(table)) {
    if (grepl("concept_id", column)) {
      table[[column]] <- as.factor(table[[column]])
    }
  }
  return(table)
}
