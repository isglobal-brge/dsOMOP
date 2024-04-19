#' Convert Concept ID Columns to Factors in the Table
#'
#' This function takes a data frame and converts all columns that contain "concept_id" in their name to factors, 
#' ignoring case.
#'
#' @param table A data frame with one or more columns that contain "concept_id" in their name.
#'
#' @return A data frame with columns that contain "concept_id" in their name converted to factors.
#'
conceptsToFactors <- function(table) {
  for (column in names(table)) {
    if (grepl("concept_id", column, ignore.case = TRUE)) {
      table[[column]] <- as.factor(table[[column]])
    }
  }
  return(table)
}


#' Convert Date Columns to Date Type in the Table
#'
#' This function takes a data frame and converts all columns that contain "_date" in their name to Date type in R, 
#' ignoring case.
#'
#' @param table A data frame with one or more columns that contain "_date" in their name.
#'
#' @return A data frame with columns that contain "_date" in their name converted to Date type.
#'
convertDateColumns <- function(table) {
  for (column in names(table)) {
    if (grepl("_date", column, ignore.case = TRUE)) {
      table[[column]] <- as.Date(table[[column]])
    }
  }
  return(table)
}
