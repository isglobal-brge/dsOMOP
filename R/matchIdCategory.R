#' @export
matchIdCategory <- function(idColumnName, idCategories) {
  for (idCategory in idCategories) {
    if (grepl(paste0(idCategory, "$"), idColumnName)) {
      return(idCategory)
    }
  }
  return(NULL)
}
