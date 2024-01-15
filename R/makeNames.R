#' @export
makeNames <- function(name) {
  name <- make.names(name)
  name <- tolower(name)
  name <- gsub("\\.", "_", name)
  name <- gsub("_+", "_", name)
  name <- gsub("^_|_$", "", name)
  return(name)
}
