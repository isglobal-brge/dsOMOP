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
#' This function takes a data frame and checks each column name by splitting it at each period ('.'). 
#' It then converts columns to DateTime type in R if "_datetime" is found in any part of the name except the first token,
#' and to Date type if "_date" is found under the same conditions, ignoring case.
#'
#' @param table A data frame with one or more columns that may contain "_date" or "_datetime" in their name.
#'
#' @return A data frame with columns that contain "_datetime" converted to DateTime type and columns that contain "_date" 
#' converted to Date type.
#'
convertDateColumns <- function(table) {
  for (column in names(table)) {
    tokens <- unlist(strsplit(column, ".", fixed = TRUE))
    if (length(tokens) > 1) { # Ensure there's more than one token
      tokens <- tokens[-1] # Remove the first token
      joined_tokens <- paste(tokens, collapse = ".") # Re-join the tokens excluding the first one
      if (grepl("_datetime", joined_tokens, ignore.case = TRUE)) {
        table[[column]] <- as.POSIXct(table[[column]])
      } else if (grepl("_date", joined_tokens, ignore.case = TRUE)) {
        table[[column]] <- as.Date(table[[column]])
      }
    }
  }
  return(table)
}
