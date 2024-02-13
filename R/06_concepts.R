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

getConceptIdColumns <- function(table) {
  conceptIdColumns <- names(table)[grepl("_concept_id", names(table))]
  return(conceptIdColumns)
}

getConceptIds <- function(table, conceptIdColumns) {
  conceptIds <- unique(unlist(lapply(conceptIdColumns, function(col) na.omit(table[[col]]))))
  return(conceptIds)
}

getConcepts <- function(connection, conceptIds) {
  query <- sprintf("SELECT concept_id, concept_name FROM concept WHERE concept_id IN (%s)", 
                   paste(conceptIds, collapse = ", "))
  result <- DBI::dbGetQuery(connection, query)  
  return(result)
}

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
