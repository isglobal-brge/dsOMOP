#' Convert String Columns to Factors in the Table
#'
#' This function takes a data frame and converts all columns of type character to factors.
#'
#' @param table A data frame with one or more columns of type character.
#'
#' @return A data frame with character columns converted to factors.
#'
stringsToFactors <- function(table) {
  for (column in names(table)) {
    if (is.character(table[[column]])) {
      table[[column]] <- as.factor(table[[column]])
    }
  }
  return(table)
}
