#' Translate Table Concepts
#'
#' This function translates concept IDs in a table to their corresponding concept names. 
#' 
#' It first checks if the 'concept' table exists in the database. If it does, the function identifies columns containing 
#' concept IDs, retrieves these IDs, and then fetches the corresponding concept names from the 'concept' table. 
#' 
#' Finally, it replaces the concept IDs in the original table with the fetched concept names.
#'
#' @param connection A DBI database connection object.
#' @param table A data frame representing the table with concept IDs to be translated.
#'
#' @return A data frame with concept IDs replaced by their corresponding concept names.
#'
translateTable <- function(connection, table) {
  # Checks if the 'concept' table exists in the database
  if (!DBI::dbExistsTable(connection, "concept")) {
    return(table)
  }

  # Retrieves the concept IDs from the table
  conceptIdColumns <- getConceptIdColumns(table)
  conceptIds <- getConceptIds(table, conceptIdColumns)

  # If there are concept IDs, retrieves the concept names and translates them
  if (!is.null(conceptIds) && length(conceptIds) > 0) {
    concepts <- getConcepts(connection, conceptIds)
    table <- translateConcepts(table, conceptIdColumns, concepts)
  }
  return(table)
}


#' Get Concept ID Columns from a Table
#'
#' This function examines each column in a given table to identify those that contain concept IDs. 
#' Concept ID columns are recognized by the presence of "_concept_id" in their names. These columns 
#' are essential for the translation process, where concept IDs are replaced with their corresponding 
#' concept names.
#'
#' @param table A data frame representing the table to be examined.
#'
#' @return A character vector containing the names of the columns that include concept IDs.
#'
getConceptIdColumns <- function(table) {
  conceptIdColumns <- names(table)[grepl("_concept_id", names(table))]
  return(conceptIdColumns)
}


#' Get Concept IDs from a Table
#'
#' This function extracts all unique concept IDs from specified concept ID columns within a table. It iterates
#' over each concept ID column, collects all unique concept IDs while omitting any NA values, and returns a 
#' vector of these unique concept IDs. This is crucial for identifying the distinct concepts present in the table
#' that may require translation or further processing.
#'
#' @param table A data frame representing the table from which to extract concept IDs.
#' @param conceptIdColumns A character vector specifying the names of the columns that contain concept IDs.
#'
#' @return A numeric vector containing all unique concept IDs from the specified concept ID columns.
#'
getConceptIds <- function(table, conceptIdColumns) {
  conceptIds <- unique(unlist(lapply(conceptIdColumns, function(column) na.omit(table[[column]]))))
  return(conceptIds)
}


#' Get Concept Names from Concept IDs
#'
#' This function queries the "concept" table to find the corresponding concept names for a given list of concept IDs. 
#' It constructs a dictionary of concept IDs and their associated names, which is essential for translating concept IDs 
#' in tables to their meaningful names.
#'
#' @param connection A database connection object through which the query will be executed.
#' @param conceptIds A numeric vector containing the concept IDs for which names are to be retrieved.
#'
#' @return A data frame with columns "concept_id" and "concept_name", representing the mapping from concept IDs to their names.
#'
getConcepts <- function(connection, conceptIds) {
  query <- sprintf("SELECT concept_id, concept_name FROM concept WHERE concept_id IN (%s)", 
                   paste(conceptIds, collapse = ", "))

  concepts <- DBI::dbGetQuery(connection, query)  
  return(concepts)
}


#' Replace Concept IDs with Concept Names in a Table
#'
#' This function replaces concept IDs with their corresponding concept names within a table. For each column that contains concept IDs,
#' it substitutes the concept ID values with the concept names using a provided dictionary (concepts) obtained from the "concept" table
#' in the database. This is essential for converting concept IDs into a more human-readable format.
#'
#' @param table A data frame representing the table in which concept IDs are to be translated.
#' @param conceptIdColumns A character vector specifying the names of the columns that contain concept IDs.
#' @param concepts A data frame serving as a dictionary, mapping concept IDs to concept names, obtained from the "concept" table.
#'
#' @return A data frame with concept IDs replaced by concept names in the specified columns.
#'
translateConcepts <- function(table, conceptIdColumns, concepts) {
  for (column in conceptIdColumns) {
    table[[column]] <- sapply(table[[column]], function(id) {
      name <- concepts$concept_name[match(id, concepts$concept_id)]
      if (is.na(name)) {
        return(id) # If the concept ID is not found, returns the original value
      } else {
        return(name)
      }
    })
  }
  return(table)
}
