#' @title Sorts the columns of a table based on their category.
#' @description This function reorders the columns of a table such that columns without a dot in their name come first,
#' followed by columns with a dot in their name. Within these two categories, columns are further sorted
#' based on their "_id" suffix. Columns with the same "_id" suffix are grouped together.
#' @param table A data frame representing the table to be sorted.
#' @return A data frame representing the sorted table.
sortTableCategories <- function(table) {
  # Get the names of all columns in the table
  colNames <- colnames(table)
  # Separate column names into those with and without a dot
  noDotNames <- colNames[!grepl("\\.", colNames)]
  dotNames <- colNames[grepl("\\.", colNames)]
  # Create a new order for the columns, with no-dot names first
  newOrder <- c(noDotNames, dotNames)
  # Identify columns with "_id" in their name
  idNames <- grep("_id", newOrder, value = TRUE)
  # For each "_id" column, group it with columns that have the same prefix
  for (idName in idNames) {
    # Split the column name at "_id"
    splitName <- unlist(strsplit(idName, "_id", fixed = TRUE))
    # Get the part of the name before the last "_id"
    if (length(splitName) > 1) {
      partBeforeLastId <- paste(splitName[-length(splitName)], collapse = "_id")
    } else {
      partBeforeLastId <- splitName
    }
    # Find the position of the "_id" column in the new order
    idNamePosition <- which(newOrder == idName)
    # Get the names of all columns that follow the "_id" column in the new order
    followingNames <- newOrder[idNamePosition:length(newOrder)]
    # Identify columns that start with the same prefix as the "_id" column
    matchingNames <- followingNames[startsWith(followingNames, partBeforeLastId)]
    # Remove these columns from their current position in the new order
    newOrder <- newOrder[!newOrder %in% matchingNames]
    # Add them back to the new order, immediately after the "_id" column
    newOrder <- append(newOrder, matchingNames, after = idNamePosition - 1)
  }
  # Reorder the columns of the table according to the new order
  table <- table[, newOrder]
  return(table)
}
