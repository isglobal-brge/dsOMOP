#' @title Match ID column name to a category
#' @description This function matches a given ID column name to a category from a list of categories.
#' The matching is done based on whether the ID column name ends with the category name.
#' The function uses the 'grepl' function to match the pattern at the end of the ID column name.
#' If a match is found, the category name is returned. If no match is found, NULL is returned.
#' @param idColumnName A character string representing the name of the ID column.
#' @param idCategories A character vector representing the list of categories to match the ID column name against.
#' @return A character string representing the matched category, or NULL if no match is found.
matchIdCategory <- function(idColumnName, idCategories) {
  for (idCategory in idCategories) {
    # If the ID column name ends with the category name, return the category
    if (grepl(paste0(idCategory, "$"), idColumnName)) {
      return(idCategory)
    }
  }
  # If no match is found, return NULL
  return(NULL)
}
