getPersonIds <- function(tableName) {
  # Checks if the given table exists in the environment and retrieves it
  if (!exists(tableName, envir = .GlobalEnv)) {
    stop(paste0("The table '", tableName, "' does not exist in the environment."))
  }
  table <- get(tableName, envir = .GlobalEnv)

  # Checks if the table has a 'person_id' column
  if (!"person_id" %in% names(table)) {
    stop(paste0("The table '", tableName, "' does not have a 'person_id' column."))
  }
  
  # Retrieves the unique values of the 'person_id' column
  personIds <- unique(table$person_id)
  return(personIds)
}

getSubsetFilter <- function() {
  nFilter <- dsBase::listDisclosureSettingsDS()
  subsetFilter <- nFilter$nfilter.subset
  return(subsetFilter)
}
