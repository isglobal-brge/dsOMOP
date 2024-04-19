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


#' Convert Date and DateTime Columns in the Table
#'
#' This function takes a data frame and converts all columns that contain "_datetime" in their name to DateTime type in R,
#' and all columns that contain "_date" in their name to Date type, ignoring case.
#'
#' @param table A data frame with one or more columns that may contain "_date" or "_datetime" in their name.
#'
#' @return A data frame with columns that contain "_datetime" converted to DateTime type and columns that contain "_date" 
#' converted to Date type.
#'
convertDateColumns <- function(table) {
  for (column in names(table)) {
    if (grepl("_datetime", column, ignore.case = TRUE)) {
      table[[column]] <- as.POSIXct(table[[column]])
    } else if (grepl("_date", column, ignore.case = TRUE)) {
      table[[column]] <- as.Date(table[[column]])
    }
  }
  return(table)
}
