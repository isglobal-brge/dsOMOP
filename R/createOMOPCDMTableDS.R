#' @title Create an OMOP CDM table for DataSHIELD
#' @description This function is called from the DataSHIELD client to the server to retrieve an OMOP CDM table.
#' It creates a table from the provided resource, with options to remove concept IDs and/or NA values.
#' @param resource An object of class 'OMOPCDMResourceClient' representing the OMOP CDM database resource.
#' @param removeConceptId A logical indicating whether to remove concept IDs from the table. Default is TRUE.
#' @param removeNA A logical indicating whether to remove NA values from the table. Default is FALSE.
#' @return A table object representing the OMOP CDM table.
#' @export
createOMOPCDMTableDS <- function(resource, removeConceptId = TRUE, removeNA = FALSE) {
  # Create the table from the resource, with options to remove concept IDs and/or NA values
  table <- createTable(resource, removeConceptId = removeConceptId, removeNA = removeNA)
  return(table)
}
