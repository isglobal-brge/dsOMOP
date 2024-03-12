#' Standardize Name
#'
#' This function standardizes a given name to ensure compatibility with the translation process.
#' It performs the following operations:
#' 1. Converts the name to a valid R object name (make.names)
#' 2. Changes all characters to lowercase
#' 3. Replaces periods with underscores
#' 4. Removes repeated underscores
#' 5. Trims leading and trailing underscores
#'
#' @param name A character string representing the name to be standardized.
#'
#' @return A character string representing the standardized name.
#'
standardizeName <- function(name) {
  name <- make.names(name) # Converts names into valid R object names
  name <- tolower(name) # Converts all characters to lowercase
  name <- gsub("\\.", "_", name) # Replaces periods with underscores
  name <- gsub("_+", "_", name) # Replaces repeated underscores with a single one
  name <- gsub("^_|_$", "", name) # Removes leading and trailing underscores
  return(name)
}
