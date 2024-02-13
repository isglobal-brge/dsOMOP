standardizeName <- function(name) {
  name <- make.names(name)  # Converts names into valid R object names
  name <- tolower(name)  # Converts all characters to lowercase
  name <- gsub("\\.", "_", name)  # Replaces periods with underscores
  name <- gsub("_+", "_", name)  # Replaces repeated underscores with a single one
  name <- gsub("^_|_$", "", name)  # Removes leading and trailing underscores
  return(name)
}

standardizeColumn <- function(table, column) {
  table[[column]] <- sapply(table[[column]], standardizeName)
  return(table)
}