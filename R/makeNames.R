#' @title Standardize names for R objects with underscores
#' @description This function takes a name and standardizes it to make it able to be a column name in a data frame.
#' It converts the name to lower case, replaces periods with underscores, and removes leading and trailing underscores.
#' The purpose of this function is to ensure that the translated concept names can be reshaped into column names.
#' @param name A character string representing the name to be standardized.
#' @return A character string representing the standardized name.
makeNames <- function(name) {
  # Convert the name to a valid R object name
  name <- make.names(name)
  # Convert the name to lower case
  name <- tolower(name)
  # Replace periods with underscores
  name <- gsub("\\.", "_", name)
  # Replace multiple underscores with a single underscore
  name <- gsub("_+", "_", name)
  # Remove leading and trailing underscores
  name <- gsub("^_|_$", "", name)
  return(name)
}
