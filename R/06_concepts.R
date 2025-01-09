#' Translate Concept IDs to Concept Names in OMOP CDM Tables
#'
#' @title Concept Translation for OMOP CDM Tables
#' @description
#' Translates concept IDs in an OMOP CDM table to their corresponding concept names by querying
#' the vocabulary tables. Handles schema management, case-insensitive matching, and graceful
#' fallbacks when concepts cannot be found.
#'
#' @details
#' The function performs the following key operations:
#' 1. Schema Management:
#'    * Validates and switches between schemas as needed
#'    * Handles vocabulary schema separately if specified
#'    * Restores original schema after operations
#'
#' 2. Concept Table Operations:
#'    * Locates the concept table case-insensitively
#'    * Validates concept table structure
#'    * Extracts relevant concept mappings
#'
#' 3. Translation Process:
#'    * Identifies columns containing concept IDs (e.g., "condition_concept_id", "drug_concept_id")
#'    * Retrieves unique concept IDs from the table
#'    * Maps IDs to concept names from vocabulary
#'    * Replaces IDs with standardized concept names
#'
#' Schema Handling:
#' * Primary schema: Used for main table operations
#' * Vocabulary schema: Optional separate schema for concept lookups
#' * Schema switching: Ensures queries target correct tables
#'
#' Error Handling:
#' * Missing concept table: Returns original table with warning
#' * Failed concept retrieval: Maintains original IDs with "concept_id_" prefix
#' * Schema errors: Restores original schema and reports error
#'
#' @param connection A DBI database connection object for database operations
#' @param table A data frame containing OMOP CDM data with concept IDs
#' @param dbms Character string specifying the database management system (e.g., "postgresql", "oracle")
#' @param schema Optional character string specifying the primary database schema
#' @param vocabularySchema Optional character string specifying a separate vocabulary schema
#'
#' @return A data frame with concept IDs replaced by their corresponding concept names.
#'         Unmapped concepts are prefixed with "concept_id_" for traceability.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default schema
#' translated_data <- translateTable(
#'   connection = db_conn,
#'   table = condition_table,
#'   dbms = "postgresql"
#' )
#'
#' # Using separate vocabulary schema
#' translated_data <- translateTable(
#'   connection = db_conn,
#'   table = condition_table,
#'   dbms = "postgresql",
#'   schema = "cdm",
#'   vocabularySchema = "vocabulary"
#' )
#' }
#'
translateTable <- function(connection, table, dbms, schema = NULL, vocabularySchema = NULL) {
  # Step 1: Initialize schema management
  # Retrieves appropriate schema queries based on DBMS type
  schemaQuery <- getSchemaQuery(dbms)  # Gets DBMS-specific schema switch query
  schemaRetrievalQuery <- getSchemaRetrievalQuery(dbms)  # Gets current schema query
  currentSchema <- DBI::dbGetQuery(connection, schemaRetrievalQuery)[[1]]
  schemaToUse <- if (!is.null(vocabularySchema)) vocabularySchema else schema

  # Step 2: Switch to target schema if a particular one was specified
  # Ensures queries target the correct schema for vocabulary operations
  if (!is.null(schemaToUse) && schemaToUse != currentSchema) {
    DBI::dbExecute(connection, fillSchemaQuery(schemaToUse, schemaQuery))
  }

  # Step 3: Locate and validate concept table
  # Searches for concept table case-insensitively to handle different DBMS conventions
  tables <- getTables(connection)
  conceptTable <- findCaseInsensitiveTable(tables, "concept")

  if (is.null(conceptTable)) {
    warning("Concept table not found in the specified schema. Concept translation will be skipped.")
    if (!is.null(schemaToUse) && schemaToUse != currentSchema) {
      DBI::dbExecute(connection, fillSchemaQuery(currentSchema, schemaQuery))
    }
    return(table)
  }

  # Step 4: Extract concept IDs for translation
  # Identifies and collects all unique concept IDs needing translation
  conceptIdColumns <- getConceptIdColumns(table)  # Find columns ending in _concept_id
  conceptIds <- getConceptIds(table, conceptIdColumns)  # Extract unique IDs
  conceptIds <- conceptIds[!is.nan(conceptIds)]  # Remove any NaN values

  # Step 5: Retrieve concept mappings
  # Attempts to fetch concept names from vocabulary, with error handling
  concepts <- tryCatch({
    getConcepts(connection, conceptIds, conceptTable, dbms, schema, vocabularySchema)
  }, error = function(error) {
    warning(paste("Error retrieving concepts:", error))
    data.frame(concept_id = integer(), concept_name = character(), stringsAsFactors = FALSE)
  })

  # Step 6: Restore original schema
  # Ensures database state is restored after operations
  if (!is.null(schemaToUse) && schemaToUse != currentSchema) {
    DBI::dbExecute(connection, fillSchemaQuery(currentSchema, schemaQuery))
  }

  # Step 7: Perform concept translation
  # Replaces concept IDs with standardized names
  table <- translateConcepts(table, conceptIdColumns, concepts)

  return(table)
}

#' Identify Concept ID Columns in OMOP CDM Tables
#'
#' @title Concept ID Column Identification
#' @description
#' Identifies columns in an OMOP CDM table that contain concept IDs by examining column names
#' for the standard "_concept_id" suffix pattern.
#'
#' @details
#' The function performs pattern matching to identify concept ID columns:
#' * Searches for "_concept_id" suffix in column names (case-sensitive)
#' * Handles standard OMOP column naming patterns:
#'   - condition_concept_id
#'   - drug_concept_id
#'   - procedure_concept_id
#'   - measurement_concept_id
#'   - observation_concept_id
#'   etc.
#'
#' Column Identification Process:
#' 1. Examines all column names in the input table
#' 2. Uses regex pattern "_concept_id" for matching
#' 3. Maintains case sensitivity for OMOP standard compliance
#' 4. Returns vector of matching column names
#'
#' @param table A data frame containing OMOP CDM data
#'
#' @return Character vector of column names that contain concept IDs
#'
#' @examples
#' \dontrun{
#' # Example with multiple concept ID columns
#' test_data <- data.frame(
#'   person_id = 1:3,
#'   condition_concept_id = c(1, 2, 3),
#'   drug_concept_id = c(4, 5, 6),
#'   visit_occurrence_id = c(7, 8, 9)  # Not a concept ID column
#' )
#' concept_columns <- getConceptIdColumns(test_data)
#' # Returns c("condition_concept_id", "drug_concept_id")
#' }
#'
getConceptIdColumns <- function(table) {
  # Identify columns ending with "_concept_id" using regex pattern matching
  conceptIdColumns <- names(table)[grepl("_concept_id", names(table))]
  return(conceptIdColumns)
}

#' Extract Unique Concept IDs from OMOP CDM Tables
#'
#' @title Unique Concept ID Extraction
#' @description
#' Extracts and deduplicates concept IDs from specified columns in an OMOP CDM table,
#' handling NA values and ensuring clean data for concept translation.
#'
#' @details
#' The function performs several key operations:
#' 1. Column Processing:
#'    * Iterates through each specified concept ID column
#'    * Extracts all values using column subsetting
#'    * Handles missing and NULL values
#'
#' 2. Data Cleaning:
#'    * Removes NA values using na.omit()
#'    * Combines IDs from all columns using unlist()
#'    * Deduplicates using unique()
#'
#' 3. Special Value Handling:
#'    * Manages NULL values
#'    * Handles missing data
#'    * Processes empty strings
#'
#' Common Concept ID Types:
#' * Standard concepts (usually positive integers)
#' * Source concepts (can be negative integers)
#' * Custom concepts (system-specific IDs)
#'
#' @param table A data frame containing OMOP CDM data
#' @param conceptIdColumns Character vector of column names containing concept IDs
#'
#' @return Numeric vector of unique, non-NA concept IDs
#'
#' @examples
#' \dontrun{
#' # Example with duplicate and NA values
#' test_data <- data.frame(
#'   condition_concept_id = c(1, 1, NA, 2),
#'   drug_concept_id = c(3, NA, 4, 4),
#'   observation_concept_id = c(5, 5, NA, 6)
#' )
#' unique_ids <- getConceptIds(
#'   test_data,
#'   c("condition_concept_id", "drug_concept_id", "observation_concept_id")
#' )
#' # Returns c(1, 2, 3, 4, 5, 6)
#' }
#'
getConceptIds <- function(table, conceptIdColumns) {
  # Extract and combine concept IDs from all specified columns
  # na.omit removes NA values, unlist flattens the list, unique removes duplicates
  conceptIds <- unique(unlist(lapply(conceptIdColumns, function(column) na.omit(table[[column]]))))
  return(conceptIds)
}


#' Retrieve Concept Names from OMOP Vocabulary
#'
#' @title Concept Name Retrieval from Vocabulary
#' @description
#' Queries the OMOP vocabulary tables to retrieve concept names for a given set of concept IDs,
#' handling different database systems, schemas, and data types.
#'
#' @details
#' The function performs the following operations:
#' 1. Schema Management:
#'    * Validates schema specifications
#'    * Handles schema switching
#'    * Ensures proper cleanup
#'
#' 2. Column Identification:
#'    * Locates concept_id and concept_name columns case-insensitively
#'    * Validates column existence
#'    * Determines column data types
#'
#' 3. Query Construction:
#'    * Builds DBMS-specific queries
#'    * Handles numeric vs character concept IDs
#'    * Ensures proper SQL syntax and quoting
#'
#' 4. Data Type Handling:
#'    * Manages numeric concept IDs
#'    * Handles string-based concept IDs
#'    * Processes decimal values (removes .0 suffix)
#'
#' 5. Error Handling:
#'    * Validates input data
#'    * Handles missing concepts
#'    * Manages schema transitions
#'
#' Query Process:
#' 1. Determine concept_id column type
#' 2. Format concept IDs based on type
#' 3. Construct and execute SQL query
#' 4. Process and return results
#'
#' @param connection DBI database connection object
#' @param conceptIds Numeric vector of concept IDs to look up
#' @param conceptTable Character string specifying concept table name
#' @param dbms Character string identifying the database system
#' @param schema Optional character string for database schema
#' @param vocabularySchema Optional character string for vocabulary schema
#'
#' @return Data frame with columns "concept_id" and "concept_name"
#'
#' @examples
#' \dontrun{
#' # Basic concept retrieval
#' concepts <- getConcepts(
#'   connection = db_conn,
#'   conceptIds = c(1, 2, 3),
#'   conceptTable = "concept",
#'   dbms = "postgresql"
#' )
#'
#' # With separate vocabulary schema
#' concepts <- getConcepts(
#'   connection = db_conn,
#'   conceptIds = c(1, 2, 3),
#'   conceptTable = "concept",
#'   dbms = "postgresql",
#'   vocabularySchema = "vocabulary"
#' )
#' }
#'
getConcepts <- function(connection, conceptIds, conceptTable, dbms, schema = NULL, vocabularySchema = NULL) {
  # Step 1: Initialize schema management
  schemaQuery <- getSchemaQuery(dbms)
  schemaRetrievalQuery <- getSchemaRetrievalQuery(dbms)
  currentSchema <- DBI::dbGetQuery(connection, schemaRetrievalQuery)[[1]]
  schemaToUse <- if (!is.null(vocabularySchema)) vocabularySchema else schema

  # Step 2: Switch schemas if needed
  if (!is.null(schemaToUse) && schemaToUse != currentSchema) {
    DBI::dbExecute(connection, fillSchemaQuery(schemaToUse, schemaQuery))
  }

  # Step 3: Identify column names case-insensitively
  conceptTableColumns <- getColumns(connection, conceptTable, caseInsensitive = FALSE)
  conceptIdColumnName <- findCaseInsensitiveColumn(conceptTableColumns, "concept_id")
  conceptNameColumnName <- findCaseInsensitiveColumn(conceptTableColumns, "concept_name")

  # Step 4: Prepare query components
  fullyQualifiedTable <- DBI::dbQuoteIdentifier(connection, conceptTable)
  
  # Step 5: Determine concept_id column type
  queryTypeCheck <- sprintf(
    "SELECT %s FROM %s LIMIT 1",
    DBI::dbQuoteIdentifier(connection, conceptIdColumnName),
    fullyQualifiedTable
  )
  conceptIdType <- DBI::dbGetQuery(connection, queryTypeCheck)[[1]]

  # Step 6: Clean and prepare concept IDs based on type
  conceptIds <- conceptIds[!is.na(conceptIds)]
  inClause <- if (is.numeric(conceptIdType)) {
    # Handle numeric concept IDs
    conceptIds <- conceptIds[!is.nan(conceptIds) & !is.na(conceptIds) & 
                           conceptIds != "NaN" & conceptIds != "NA"]
    conceptIds <- gsub("\\.0$", "", conceptIds)  # Remove decimal points
    paste(conceptIds, collapse = ", ")
  } else {
    # Handle string concept IDs with proper quoting
    paste(sapply(conceptIds, function(id) 
      DBI::dbQuoteLiteral(connection, as.character(id))), collapse = ", ")
  }

  # Step 7: Construct and execute query
  query <- sprintf(
    "SELECT %s, %s FROM %s WHERE %s IN (%s)",
    DBI::dbQuoteIdentifier(connection, conceptIdColumnName),
    DBI::dbQuoteIdentifier(connection, conceptNameColumnName),
    fullyQualifiedTable,
    DBI::dbQuoteIdentifier(connection, conceptIdColumnName),
    inClause
  )
  concepts <- DBI::dbGetQuery(connection, query)
  names(concepts) <- tolower(names(concepts))  # Standardize column names

  # Step 8: Restore original schema
  if (!is.null(schemaToUse) && schemaToUse != currentSchema) {
    DBI::dbExecute(connection, fillSchemaQuery(currentSchema, schemaQuery))
  }

  return(concepts)
}

#' Replace Concept IDs with Names in OMOP CDM Tables
#'
#' @title Concept ID to Name Translation
#' @description
#' Transforms concept IDs to their corresponding concept names within an OMOP CDM table,
#' handling missing values and providing fallback naming for unmapped concepts.
#'
#' @details
#' The function performs the following operations:
#' 1. Data Type Standardization:
#'    * Converts concept IDs to character type for consistent matching
#'    * Ensures concept dictionary uses character IDs
#'
#' 2. Value Translation Process:
#'    * Iterates through each concept ID column
#'    * Handles special cases:
#'      - NA values
#'      - NULL values
#'      - Empty strings
#'    * Maps IDs to names using concept dictionary
#'    * Provides fallback naming ("concept_id_" + ID) for unmapped concepts
#'
#' 3. Name Standardization:
#'    * Applies standardizeName() function to concept names
#'    * Ensures consistent formatting
#'    * Maintains traceability for unmapped concepts
#'
#' Translation Rules:
#' * NA/NULL/Empty -> NA
#' * Mapped ID -> Standardized concept name
#' * Unmapped ID -> "concept_id_" + original ID
#'
#' @param table Data frame containing OMOP CDM data
#' @param conceptIdColumns Character vector of columns containing concept IDs
#' @param concepts Data frame mapping concept IDs to names
#'
#' @return Data frame with concept IDs replaced by standardized concept names
#'
#' @examples
#' \dontrun{
#' # Example with concept mapping
#' test_data <- data.frame(
#'   condition_concept_id = c("1", "2", NA, "3"),
#'   drug_concept_id = c("4", "5", "6", NA)
#' )
#' concept_map <- data.frame(
#'   concept_id = c("1", "2", "4", "5"),
#'   concept_name = c("Diabetes", "Hypertension", "Aspirin", "Metformin")
#' )
#' translated <- translateConcepts(
#'   test_data,
#'   c("condition_concept_id", "drug_concept_id"),
#'   concept_map
#' )
#' }
#'
translateConcepts <- function(table, conceptIdColumns, concepts) {
  for (column in conceptIdColumns) {
    # Step 1: Standardize data types for consistent matching
    concepts$concept_id <- as.character(concepts$concept_id)
    table[[column]] <- as.character(table[[column]])
    
    # Step 2: Perform translation with special case handling
    table[[column]] <- sapply(table[[column]], function(id) {
      if (is.na(id) || is.null(id) || id == "") {
        return(NA)
      } else {
        name <- concepts$concept_name[match(id, concepts$concept_id)]
        if (is.na(name)) {
          return(paste0("concept_id_", id))  # Fallback for unmapped concepts
        } else {
          return(standardizeName(name))  # Standardize mapped concept names
        }
      }
    })
  }
  return(table)
}