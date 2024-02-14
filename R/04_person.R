getPersonIds <- function(table) {
  # Checks if the table exists
  if (!exists("table")) {
    stop("The personFilter table parameter is missing or undefined.")
  }

  # Checks if the table has a 'person_id' column
  if (!"person_id" %in% names(table)) {
    stop("The personFilter table does not have a 'person_id' column.")
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
