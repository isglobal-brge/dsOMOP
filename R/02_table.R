#' Get a Table from the Database
#'
#' This function fetches a specified table from the database for use within the DataSHIELD environment. It allows users to
#' filter the table based on columns of interest, specific concept types present in the table, and person IDs from another
#' table in the system. Filtering at the database query level enhances execution times. The function also checks against
#' DataSHIELD's disclosure control to ensure that the subset operation does not return fewer patients than the minimum allowed.
#'
#' In cases where the table contains multiple elements with concept types that can be reshaped to a wide format, the function
#' will automatically reshape the table to facilitate merging with related tables. Users can specify an alternative column
#' for reshaping (default is "person_id"), useful for tables indirectly related to persons where another column serves as the
#' identifier in the reshape operation.
#'
#' Additionally, users have the option to exclude empty tables from the output, further tailoring the dataset to their needs.
#'
#' @param connection A DBI connection object representing the database connection.
#' @param tableName The name of the table to be retrieved, as a character string.
#' @param conceptFilter An optional vector of concept IDs for filtering the table.
#' @param columnFilter An optional vector specifying which columns to include in the output.
#' @param personFilter An optional vector of person IDs for filtering the table.
#' @param mergeColumn The column name to be used for merging, defaults to "person_id".
#' @param dropNA Whether to exclude columns with all NA values, defaults to FALSE.
#' @param wideLongitudinal A logical flag indicating whether to reshape the longitudinal data entries to a wide format 
#'                         with numerically suffixed columns if it detects the presence of longitudinal data, defaults 
#'                         to FALSE.
#' @param dbms An optional parameter specifying the database management system.
#' @param schema An optional parameter specifying the database schema.
#' @param vocabularySchema An optional parameter specifying the vocabulary schema.
#'
#' @return A data frame containing the filtered table, ready for integration into the DataSHIELD workflow.
#'
getTable <- function(connection,
                     tableName,
                     conceptFilter = NULL,
                     columnFilter = NULL,
                     personFilter = NULL,
                     mergeColumn = "person_id",
                     dropNA = FALSE,
                     wideLongitudinal = FALSE,
                     dbms = NULL,
                     schema = NULL,
                     vocabularySchema = NULL) {
  # Checks if the table exists in the database
  tables <- getTables(connection)
  caseInsensitiveTableName <- findCaseInsensitiveTable(tables, tableName) # Case-insensitive table search
  if (is.null(caseInsensitiveTableName)) {
    stop(paste0("The table '", tableName, "' does not exist in the database."))
  }

  # Ensures that the table name is in the correct case
  tableName <- caseInsensitiveTableName

  # Retrieves the table and its column names
  table <- dplyr::tbl(connection, tableName) %>% dplyr::rename_with(tolower)
  columns <- getColumns(connection, tableName)
  conceptIdColumn <- getConceptIdColumn(tableName)

  # Applies the column filter to the table
  keepColumns <- c("person_id", mergeColumn, conceptIdColumn)
  if (!is.null(columnFilter)) {
    columnFilter <- tolower(columnFilter) # Since all column names will be lowercase, converts the filter columns to lowercase as well
  }
  selectedColumns <- filterColumns(table, columns, columnFilter, keepColumns)
  table <- dplyr::select(table, all_of(selectedColumns))

  # Applies the concept filter to the table
  if (!is.null(conceptFilter) && conceptIdColumn %in% columns) {
    table <- dplyr::filter(table, !!sym(conceptIdColumn) %in% !!conceptFilter)
  }

  # Applies the person filter to the table
  if (!is.null(personFilter) && "person_id" %in% columns) {
    personIds <- getPersonIds(personFilter)
    table <- dplyr::filter(table, person_id %in% personIds)
  }

  # Retrieves the table as a data frame
  table <- as.data.frame(table)

  # If it is a person-related table, checks if the count of person IDs is lower than nfilter.subset
  if ("person_id" %in% columns) {
    subsetFilter <- getSubsetFilter()
    conceptIdColumn <- getConceptIdColumn(tableName)
    
    if (conceptIdColumn %in% columns) {
      # Group by concept ID and count unique person IDs for each concept
      conceptPersonCounts <- table %>%
        dplyr::group_by(!!sym(conceptIdColumn)) %>%
        dplyr::summarize(personCount = n_distinct(person_id)) %>%
        dplyr::filter(personCount >= subsetFilter)
      
      # Filter the table to exclude rows with concept IDs that don't pass the filter
      table <- table %>%
        dplyr::filter(!!sym(conceptIdColumn) %in% conceptPersonCounts[[conceptIdColumn]])
      
      # If the table is empty after the subset filter, stop the execution
      if (nrow(table) == 0) {
        stop(paste0("The resulting table is empty after applying the subset filter (nfilter.subset = ", subsetFilter, ")."))
      }
    } else {
      personCount <- length(unique(table$person_id))
      if (personCount < subsetFilter) {
        stop(paste0("The count of resulting person IDs is lower than the subset filter (nfilter.subset = ", subsetFilter, ")."))
      }
    }
  }

  # Translates the table concepts
  table <- translateTable(connection, table, vocabularySchema = vocabularySchema)

  # If a concept ID column is present, reshapes the table
  if (conceptIdColumn %in% names(table)) {
    table <- reshapeTable(table, conceptIdColumn, mergeColumn, wideLongitudinal)
  }

  # If the dropNA flag is set, removes columns with all NA values
  if (dropNA) {
    table <- table %>% select_if(~ !all(is.na(.)))
  }

  # Converts concept columns to factors
  table <- conceptsToFactors(table)

  # Converts date columns to Date type
  table <- convertDateColumns(table)

  return(table)
}


#' Assign OMOP CDM Table to the DataSHIELD Environment
#'
#' This function is designed to be called from the DataSHIELD client. It retrieves a specified table from an OMOP CDM database.
#'
#' It calls the `getTable` function, which is responsible for filtering and performing additional transformation and processing
#' operations on the table. Once processed, the table is assigned in the DataSHIELD environment.
#'
#' @param resource A resource object representing the database connection.
#' @param tableName The name of the table to be retrieved from the database.
#' @param conceptFilter (Optional) A vector of concept IDs to filter the table by specific concepts.
#' @param columnFilter (Optional) A vector of column names to keep in the table.
#' @param personFilter (Optional) A vector of person IDs to filter the table by specific individuals.
#' @param mergeColumn (Optional) The name of the column used for merging tables, defaults to "person_id".
#' @param dropNA (Optional) A logical flag indicating whether to drop columns with all NA values, defaults to FALSE.
#' @param wideLongitudinal (Optional) A logical flag indicating whether to reshape longitudinal data to a wide format,
#'                             defaults to FALSE.
#'
#' @return A data frame representing the processed table.
#'
#' @export
#'
getOMOPCDMTableDS <- function(resource,
                              tableName,
                              conceptFilter = NULL,
                              columnFilter = NULL,
                              personFilter = NULL,
                              mergeColumn = "person_id",
                              dropNA = FALSE,
                              wideLongitudinal = FALSE) {
  # Opens a connection to the database
  connection <- getConnection(resource)

  # Gets the vocabulary schema from the resource
  vocabularySchema <- resource$getVocabularySchema()

  # Attempts to retrieve the table from the database
  tryCatch(
    {
      table <- getTable(connection, tableName, conceptFilter, columnFilter, personFilter, mergeColumn, dropNA, wideLongitudinal, vocabularySchema = vocabularySchema)

      # In case of an error, closes the database connection and propagates the error
    },
    error = function(error) {
      closeConnection(connection, error)
    }
  )

  # If the retrieval was successful, closes the database connection and returns the table
  closeConnection(connection)
  return(table)
}


#' Find Case-Insensitive Table
#'
#' Given a list of table names, this function searches for a table whose name matches a given target in a case-insensitive manner.
#'
#' @param tableNames A character vector containing the names of the tables to search through.
#' @param target A character string representing the name of the table to find, case-insensitively.
#'
#' @return A character string with the name of the target table if found, otherwise NULL.
#'
findCaseInsensitiveTable <- function(tableNames, target) {
  caseInsensitiveTable <- NULL

  # Check for an exact match first
  exactMatchIndex <- match(target, tableNames)
  if (!is.na(exactMatchIndex)) {
    caseInsensitiveTable <- tableNames[exactMatchIndex]
  } else {
    # Convert both input and target to lowercase for case-insensitive comparison
    lowerTableNames <- tolower(tableNames)
    lowerTarget <- tolower(target)

    # Search for the target table in a case-insensitive manner
    matchIndex <- match(lowerTarget, lowerTableNames)

    # If a case-insensitive match is found, return the original table name
    if (!is.na(matchIndex)) {
      caseInsensitiveTable <- tableNames[matchIndex]
    }
  }

  return(caseInsensitiveTable)
}
