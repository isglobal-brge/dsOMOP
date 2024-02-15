#' Filter Columns from a Table
#'
#' This function filters columns from a given table based on the specified criteria. It allows for the inclusion
#' of mandatory columns (keepColumns) regardless of the filter applied. If no specific columns are selected,
#' it defaults to excluding columns that contain '_source' in their names.
#'
#' @param table The table from which columns are to be filtered.
#' @param tableColumns A character vector of all column names in the table.
#' @param selectedColumns A character vector specifying which columns to include in the filter.
#' @param keepColumns A character vector of columns that must be included in the output regardless of the filter.
#'
#' @return A character vector of the filtered column names.
#' 
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


#' Get Concept ID Column Name
#'
#' This function generates the name of the concept ID column based on the provided table name.
#' It removes the suffix "_occurrence" from the table name, if present, and appends "_concept_id"
#' to form the concept ID column name. This is useful for dynamically identifying the concept ID
#' column in tables where the base name of the table is used to construct column names.
#'
#' @param tableName The name of the table for which to generate the concept ID column name.
#'
#' @return A string containing the name of the concept ID column.
#' 
getConceptIdColumn <- function(tableName) {
  baseName <- gsub("_occurrence", "", tableName)
  conceptIdColumn <- paste0(baseName, "_concept_id")
  return(conceptIdColumn)
}


#' Get ID Column Name
#'
#' This function generates the name of the ID column based on the provided table name.
#' It removes the suffix "_occurrence" from the table name, if present, and appends "_id"
#' to form the ID column name. This is useful for dynamically identifying the ID column
#' in tables where the base name of the table is used to construct column names.
#'
#' @param tableName The name of the table for which to generate the ID column name.
#'
#' @return A string containing the name of the ID column.
#' 
getIdColumn <- function(tableName) {
  baseName <- gsub("_occurrence", "", tableName)
  idColumn <- paste0(baseName, "_id")
  return(idColumn)
}
