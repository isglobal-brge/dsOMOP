getTable <- function(resource,
                     tableName,
                     conceptFilter = NULL,
                     columnFilter = NULL,
                     personFilter = NULL,
                     mergeColumn = "person_id",
                     dropNA = FALSE) {
                      
  connection <- getConnection(resource)

  # Checks if the table exists in the database
  tables <- getTables(connection)
  if (!tableName %in% tables) {
    stop(paste0("The table '", tableName, "' does not exist in the database."))
  }

  # Retrieves the table and its column names
  table <- dplyr::tbl(connection, tableName)
  columns <- getColumns(connection, tableName)
  conceptIdColumn <- getConceptIdColumn(tableName)

  # Applies the column filter to the table
  keepColumns <- c("person_id", conceptIdColumn)
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

  # If the dropNA flag is set, removes columns with all NA values
  if (dropNA) {
    table <- table %>% select_if(~!all(is.na(.)))
  }
  
  # Retrieves the table as a data frame
  table <- as.data.frame(table)

  # If it is a person-related table, checks if the count of person IDs is lower than nfilter.subset
  if("person_id" %in% columns) {
    subsetFilter <- getSubsetFilter()
    personCount <- length(unique(table$person_id))
    if (personCount < subsetFilter) {
      stop(paste0("The count of resulting person IDs is lower than the subset filter (nfilter.subset = ", subsetFilter, ")."))
    }
  }

  # Translates the table concepts
  table <- translateTable(connection, table)

  # If a concept ID column is present, reshapes the table
  if(conceptIdColumn %in% names(table)) {
    table <- reshapeTable(table, conceptIdColumn, mergeColumn)
  }

  return(table)
}

#' @export
getOMOPCDMTableDS <- function(connection,
                      tableName,
                      conceptFilter = NULL,
                      columnFilter = NULL,
                      personFilter = NULL,
                      mergeColumn = "person_id",
                      dropNA = FALSE) {
  table <- getTable(connection, tableName, conceptFilter, columnFilter, personFilter, mergeColumn, dropNA)
  return(table)
}
