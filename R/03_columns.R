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
#' It removes the suffixes "_occurrence" and "_exposure" from the table name, if present, and appends "_concept_id"
#' to form the concept ID column name. This is useful for dynamically identifying the concept ID
#' column in tables where the base name of the table is used to construct column names.
#'
#' @param tableName The name of the table for which to generate the concept ID column name.
#'
#' @return A string containing the name of the concept ID column.
#'
getConceptIdColumn <- function(tableName) {
  tableName <- tolower(tableName)
  baseName <- gsub("_occurrence", "", tableName)
  baseName <- gsub("_exposure", "", baseName)
  conceptIdColumn <- paste0(baseName, "_concept_id")
  return(conceptIdColumn)
}


#' Find Case-Insensitive Column
#'
#' Given a list of column names, this function searches for a column whose name matches a given target in a case-insensitive manner.
#'
#' @param columnNames A character vector containing the names of the columns to search through.
#' @param target A character string representing the name of the column to find, case-insensitively.
#'
#' @return A character string with the name of the target column if found, otherwise NULL.
#'
findCaseInsensitiveColumn <- function(columnNames, target) {
  caseInsensitiveColumn <- NULL

  # Check for an exact match first
  exactMatchIndex <- match(target, columnNames)
  if (!is.na(exactMatchIndex)) {
    caseInsensitiveColumn <- columnNames[exactMatchIndex]
  } else {
    # Convert both input and target to lowercase for case-insensitive comparison
    lowerColumnNames <- tolower(columnNames)
    lowerTarget <- tolower(target)

    # Search for the target column in a case-insensitive manner
    matchIndex <- match(lowerTarget, lowerColumnNames)

    # If a case-insensitive match is found, return the original column name
    if (!is.na(matchIndex)) {
      caseInsensitiveColumn <- columnNames[matchIndex]
    }
  }

  return(caseInsensitiveColumn)
}
