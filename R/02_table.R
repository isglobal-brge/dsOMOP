#' Retrieve and Process OMOP CDM Table
#'
#' @title Get a Table from the OMOP CDM Database
#' @description 
#' Fetches and processes a specified table from an OMOP Common Data Model (CDM) database for use within 
#' the DataSHIELD environment. This function provides comprehensive data retrieval, filtering, and 
#' transformation capabilities while ensuring compliance with DataSHIELD's disclosure control rules.
#'
#' @details
#' The function performs several key operations:
#' 
#' 1. Table Retrieval and Validation:
#'    - Verifies table existence using case-insensitive matching
#'    - Handles schema-specific table access
#'    - Manages column name standardization
#'
#' 2. Filtering Operations:
#'    - Column filtering to select specific variables
#'    - Concept-based filtering using concept IDs
#'    - Person-level filtering using person IDs
#'    - Empty column removal (optional)
#'
#' 3. Data Type Management:
#'    - Converts concept_id columns to factors
#'    - Transforms ID columns to character type
#'    - Handles numeric and date columns appropriately
#'
#' 4. Disclosure Control:
#'    - Enforces minimum patient count thresholds
#'    - Validates subset sizes against DataSHIELD parameters
#'
#' 5. Data Reshaping:
#'    - Supports longitudinal data transformation
#'    - Handles wide/long format conversions
#'    - Manages concept translations
#'
#' @param connection A DBI connection object to the OMOP CDM database
#' @param tableName Character string specifying the target table name
#' @param conceptFilter Optional vector of concept IDs for filtering
#' @param columnFilter Optional vector of column names to include
#' @param personFilter Optional vector of person IDs for filtering
#' @param mergeColumn Character string specifying the merge key column (default: "person_id")
#' @param dropNA Logical; whether to remove columns containing only NA values (default: FALSE)
#' @param wideLongitudinal Logical; whether to reshape longitudinal data to wide format (default: FALSE)
#' @param completeTimePoints Logical; whether to ensure all entities have rows for each date point (default: FALSE)
#' @param dbms Character string specifying the database management system
#' @param schema Optional character string specifying the database schema
#' @param vocabularySchema Optional character string specifying the vocabulary schema
#' @param skipReshape Logical; whether to skip the reshaping process entirely (default: FALSE)
#'
#' @return A processed data frame containing the requested table data
#'
#' @examples
#' \dontrun{
#' # Basic table retrieval
#' conn <- getDatabaseConnection()
#' person_table <- getTable(conn, "person")
#'
#' # Filtered retrieval with concept IDs
#' condition_table <- getTable(
#'   connection = conn,
#'   tableName = "condition_occurrence",
#'   conceptFilter = c(201820, 201254),
#'   dropNA = TRUE
#' )
#'
#' # Longitudinal data with custom merge column
#' measurement_table <- getTable(
#'   connection = conn,
#'   tableName = "measurement",
#'   mergeColumn = "visit_occurrence_id",
#'   wideLongitudinal = TRUE
#' )
#'
#' # Longitudinal data with complete time points
#' lab_table <- getTable(
#'   connection = conn,
#'   tableName = "measurement",
#'   conceptFilter = c(3023314, 3024561),  # Glucose and HbA1c
#'   completeTimePoints = TRUE,
#'   wideLongitudinal = TRUE
#' )
#' }
#'
#' @seealso 
#' * \code{\link{getOMOPCDMTableDS}} for the DataSHIELD interface
#' * \code{\link{translateTable}} for concept translation details
#' * \code{\link{reshapeTable}} for data reshaping operations
#'
getTable <- function(connection,
                     tableName,
                     conceptFilter = NULL,
                     columnFilter = NULL,
                     personFilter = NULL,
                     mergeColumn = "person_id",
                     dropNA = FALSE,
                     wideLongitudinal = FALSE,
                     completeTimePoints = FALSE,
                     dbms,
                     schema = NULL,
                     vocabularySchema = NULL,
                     skipReshape = FALSE) {
  # Checks if the table exists in the database
  tables <- getTables(connection)
  caseInsensitiveTableName <- findCaseInsensitiveTable(tables, tableName)
  if (is.null(caseInsensitiveTableName)) {
    stop(paste0("The table '", tableName, "' does not exist in the database."))
  }
  tableName <- caseInsensitiveTableName

  # Step 2: Table Retrieval and Initial Processing
  # Create table reference with schema handling
  table <- if (!is.null(schema)) {
    dplyr::tbl(connection, dbplyr::in_schema(schema, tableName))
  } else {
    dplyr::tbl(connection, tableName)
  }
  table <- table %>% dplyr::rename_with(tolower)
  
  # Get column information
  columns <- getColumns(connection, tableName)
  conceptIdColumn <- getConceptIdColumn(tableName)

  # Step 3: Column Selection
  keepColumns <- c("person_id", mergeColumn, conceptIdColumn)
  if (!is.null(columnFilter)) {
    columnFilter <- tolower(columnFilter)
  }
  selectedColumns <- filterColumns(table, columns, columnFilter, keepColumns)
  table <- dplyr::select(table, all_of(selectedColumns))

  # Step 4: Apply Filters
  # Concept filtering
  if (!is.null(conceptFilter) && conceptIdColumn %in% columns) {
    table <- dplyr::filter(table, !!sym(conceptIdColumn) %in% !!conceptFilter)
  }

  # Person filtering
  if (!is.null(personFilter) && "person_id" %in% columns) {
    personIds <- getPersonIds(personFilter)
    table <- dplyr::filter(table, person_id %in% personIds)
  }

  # Step 5: Data Collection
  table <- tryCatch({
    as.data.frame(table)
  }, error = function(e) {
    # Fallback collection method
    table <- dplyr::tbl(connection, tableName) %>% 
      dplyr::rename_with(tolower) %>%
      dplyr::select(all_of(selectedColumns)) %>%
      dplyr::collect() %>%
      as.data.frame()
    
    # Reapply filters after collection
    if (!is.null(conceptFilter) && conceptIdColumn %in% columns) {
      table <- table[table[[conceptIdColumn]] %in% conceptFilter, ]
    }
    if (!is.null(personFilter) && "person_id" %in% columns) {
      table <- table[table$person_id %in% personIds, ]
    }
    table
  })

  # Step 6: Data Type Conversions
  # Convert concept_id columns to factors
  concept_id_cols <- grep("concept_id$", names(table), value = TRUE)
  if (length(concept_id_cols) > 0) {
    table <- table %>%
      dplyr::mutate(across(all_of(concept_id_cols), ~as.factor(.)))
  }

  # Convert ID columns to character
  id_cols <- grep("_id$", names(table), value = TRUE)
  id_cols <- id_cols[!grepl("concept_id$", id_cols)]
  if (length(id_cols) > 0) {
    table <- table %>%
      dplyr::mutate(across(all_of(id_cols), ~as.character(.)))
  }
  
  # Convert numeric columns
  number_cols <- grep("_as_number$|^range_low$|^range_high$", names(table), value = TRUE)
  if (length(number_cols) > 0) {
    table <- table %>%
      dplyr::mutate(across(all_of(number_cols), ~as.numeric(as.character(.))))
  }

  # Step 7: Disclosure Control Checks
  if ("person_id" %in% columns) {
    subsetFilter <- getSubsetFilter()
    conceptIdColumn <- getConceptIdColumn(tableName)
    
    if (conceptIdColumn %in% columns) {
      # Check counts by concept
      conceptPersonCounts <- table %>%
        dplyr::group_by(!!sym(conceptIdColumn)) %>%
        dplyr::summarize(personCount = n_distinct(person_id)) %>%
        dplyr::filter(personCount >= subsetFilter)
      
      table <- table %>%
        dplyr::filter(!!sym(conceptIdColumn) %in% conceptPersonCounts[[conceptIdColumn]])
      
      if (nrow(table) == 0) {
        stop(paste0("Empty result after subset filter (nfilter.subset = ", subsetFilter, ")."))
      }
    } else {
      # Check total person count
      personCount <- length(unique(table$person_id))
      if (personCount < subsetFilter) {
        stop(paste0("Person count below subset filter (nfilter.subset = ", subsetFilter, ")."))
      }
    }
  }

  # Step 8: Final Processing
  # Translate concepts
  table <- translateTable(connection, table, dbms, schema, vocabularySchema)

  # If a concept ID column is present and reshaping is not explicitly skipped, reshapes the table
  if (!skipReshape && conceptIdColumn %in% names(table)) {
    table <- reshapeTable(
      table, 
      conceptIdColumn, 
      mergeColumn, 
      wideLongitudinal, 
      completeTimePoints
    )
  }

  # Remove empty columns if requested
  if (dropNA) {
    table <- table %>% select_if(~ !all(is.na(.)))
  }

  # Final data type conversions
  table <- conceptsToFactors(table)
  table <- convertDateColumns(table)

  return(table)
}

#' DataSHIELD Interface for OMOP CDM Table Retrieval
#'
#' @title Assign OMOP CDM Table to DataSHIELD Environment
#' @description 
#' Provides a DataSHIELD-compliant interface for retrieving and processing OMOP CDM tables. This function
#' manages the database connection lifecycle and ensures proper error handling while delegating the actual
#' table processing to the \code{getTable} function.
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
#' @param completeTimePoints (Optional) A logical flag indicating whether to ensure all entities have rows for each date point,
#'                             defaults to FALSE.
#' @param skipReshape (Optional) A logical flag indicating whether to skip reshaping the table, defaults to FALSE.
#'
#' @return A data frame representing the processed table.
#'
#' @export
getOMOPCDMTableDS <- function(resource,
                              tableName,
                              conceptFilter = NULL,
                              columnFilter = NULL,
                              personFilter = NULL,
                              mergeColumn = "person_id",
                              dropNA = FALSE,
                              wideLongitudinal = FALSE,
                              completeTimePoints = FALSE,
                              skipReshape = FALSE) {
  # Opens a connection to the database
  connection <- getConnection(resource)

  # Step 2: Schema Information Retrieval
  vocabularySchema <- resource$getVocabularySchema()
  dbms <- resource$getDBMS()
  schema <- resource$getSchema()

  # Step 3: Table Processing with Error Handling
  tryCatch(
    {
      table <- getTable(connection, tableName, conceptFilter, columnFilter, personFilter, mergeColumn, dropNA, wideLongitudinal, completeTimePoints, dbms, schema, vocabularySchema, skipReshape)

      # In case of an error, closes the database connection and propagates the error
    },
    error = function(error) {
      closeConnection(connection, error)
    }
  )

  # Step 4: Cleanup
  closeConnection(connection)
  return(table)
}

#' Case-Insensitive Table Name Matching
#'
#' @title Find Table Name with Case-Insensitive Matching
#' @description 
#' Performs case-insensitive matching of table names to support flexible table name references.
#' Prioritizes exact matches before falling back to case-insensitive matching.
#'
#' @param tableNames Character vector of available table names
#' @param target Character string of the target table name to find
#'
#' @return Character string of the matched table name or NULL if no match found
#'
#' @examples
#' \dontrun{
#' tables <- c("Person", "Measurement", "Observation")
#' 
#' # Exact match
#' findCaseInsensitiveTable(tables, "Person")  # Returns "Person"
#'
#' # Case-insensitive match
#' findCaseInsensitiveTable(tables, "person")  # Returns "Person"
#'
#' # No match
#' findCaseInsensitiveTable(tables, "Drug")    # Returns NULL
#' }
#'
findCaseInsensitiveTable <- function(tableNames, target) {
  # Step 1: Try exact match first
  exactMatchIndex <- match(target, tableNames)
  if (!is.na(exactMatchIndex)) {
    return(tableNames[exactMatchIndex])
  }

  # Step 2: Try case-insensitive match
  lowerTableNames <- tolower(tableNames)
  lowerTarget <- tolower(target)
  matchIndex <- match(lowerTarget, lowerTableNames)
  
  # Step 3: Return result
  if (!is.na(matchIndex)) {
    return(tableNames[matchIndex])
  }
  
  return(NULL)
}
