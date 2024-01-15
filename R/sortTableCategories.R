#' @export
sortTableCategories <- function(table) {
  colNames <- colnames(table)
  noDotNames <- colNames[!grepl("\\.", colNames)]
  dotNames <- colNames[grepl("\\.", colNames)]
  newOrder <- c(noDotNames, dotNames)
  idNames <- grep("_id", newOrder, value = TRUE)
  for (idName in idNames) {
    splitName <- unlist(strsplit(idName, "_id", fixed = TRUE))
    if (length(splitName) > 1) {
      partBeforeLastId <- paste(splitName[-length(splitName)], collapse = "_id")
    } else {
      partBeforeLastId <- splitName
    }
    idNamePosition <- which(newOrder == idName)
    followingNames <- newOrder[idNamePosition:length(newOrder)]
    matchingNames <- followingNames[startsWith(followingNames, partBeforeLastId)]
    newOrder <- newOrder[!newOrder %in% matchingNames]
    newOrder <- append(newOrder, matchingNames, after = idNamePosition - 1)
  }
  table <- table[, newOrder]
  return(table)
}
