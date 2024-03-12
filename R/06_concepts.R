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
  tables <- getTables(connection)
  conceptTable <- findCaseInsensitiveTable(tables, "concept")

  # Retrieves the concept IDs from the table
  conceptIdColumns <- getConceptIdColumns(table)
  conceptIds <- getConceptIds(table, conceptIdColumns)

  # Attempts to retrieve the concept names from the 'concept' table
  concepts <- tryCatch(
    {
      getConcepts(connection, conceptIds, conceptTable)
    },
    # In case of an error, returns an empty data frame (with the same structure as the expected output)
    error = function(error) {
      data.frame(concept_id = integer(), concept_name = character(), stringsAsFactors = FALSE)
    }
  )

  # Translates the concept IDs in the table
  table <- translateConcepts(table, conceptIdColumns, concepts)

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
#' This function queries the specified "concept" table to find the corresponding concept names for a given list of concept IDs.
#' It constructs a dictionary of concept IDs and their associated names, which is essential for translating concept IDs
#' in tables to their meaningful names.
#'
#' @param connection A database connection object through which the query will be executed.
#' @param conceptIds A numeric vector containing the concept IDs for which names are to be retrieved.
#' @param conceptTable A character string specifying the exact name of the concept table to be queried.
#'
#' @return A data frame with columns "concept_id" and "concept_name", representing the mapping from concept IDs to their names.
#'
getConcepts <- function(connection, conceptIds, conceptTable) {
  query <- sprintf(
    "SELECT concept_id, concept_name FROM \"%s\" WHERE concept_id IN (%s)",
    conceptTable,
    paste(conceptIds, collapse = ", ")
  )

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
    # Attempts to replace concept IDs with their corresponding concept names
    table[[column]] <- sapply(table[[column]], function(id) {
      # If the ID is NA, NULL, or empty, returns NA
      if (is.na(id) || is.null(id) || id == "") {
        return(NA)
      } else {
        # Otherwise, attempts to find the concept name for the given concept ID
        name <- concepts$concept_name[match(id, concepts$concept_id)]
        if (is.na(name)) {
          # If the concept name is not found, returns "concept_id_" + the concept ID
          return(paste0("concept_id_", id))
        } else {
          # Otherwise, returns the corresponding concept name (standardized)
          return(standardizeName(name))
        }
      }
    })
  }
  return(table)
}
