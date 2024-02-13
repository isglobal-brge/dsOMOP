filterColumns <- function(table, tableColumns, selectedColumns, keepColumns) {
  # If no column filter is provided, all columns are included except source value ones
  if (is.null(selectedColumns)) {
    selectedColumns <- tableColumns[!grepl("_source", tableColumns)]
    return(selectedColumns)
  }

  # Ensures all mandatory columns are also included
  if (!is.null(keepColumns)) {
    selectedColumns <- unique(c(selectedColumns, keepColumns[keepColumns %in% tableColumns]))
  }
  
  filteredColumns <- tableColumns[tableColumns %in% selectedColumns]
  return(filteredColumns)
}

getConceptIdColumn <- function(tableName) {
  baseName <- gsub("_occurrence", "", tableName)
  conceptIdColumn <- paste0(baseName, "_concept_id")
  return(conceptIdColumn)
}

getIdColumn <- function(tableName) {
  baseName <- gsub("_occurrence", "", tableName)
  idColumn <- paste0(baseName, "_id")
  return(idColumn)
}
