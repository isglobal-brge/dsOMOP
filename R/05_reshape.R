reshapeTable <- function(table, conceptIdColumn, mergeColumn) {
  # Checks if the merge column is present in the table
  if (!mergeColumn %in% names(table)) {
    stop(paste0("The column '", mergeColumn, "' is not present in the table."))
  }

  # Standardizes the concept ID column name to ensure that the reshaping process works
  table <- standardizeColumn(table, conceptIdColumn)

  # Sequences repeated data in the concept ID colum in case there is longitudinal data
  table <- sequenceColumn(table, conceptIdColumn, mergeColumn)

  table <- dsBase::reShapeDS(
    data.name = "table",
    varying.transmit = NULL,
    v.names.transmit = names(table)[!names(table) %in% c(mergeColumn, conceptIdColumn)],
    timevar.name = conceptIdColumn,
    idvar.name = mergeColumn,
    drop.transmit = NULL,
    direction = "wide",
    sep = "."
  )

  # Rearranges the column name structure for better readability
  table <- rearrangeColumnNames(table)
  return(table)
}

rearrangeColumnNames <- function(table) {
  # Splits the column names into tokens and rearranges them
  tokens <- strsplit(names(table), "\\.")
  rearrangedTokens <- lapply(tokens, function(token) {
    if (length(token) > 1) {
      token <- c(token[-1], token[1])
    }
    return(token)
  })

  # Joins the rearranged tokens back together to form the new column names
  newColumns <- sapply(rearrangedTokens, paste, collapse = ".")
  names(table) <- newColumns
  return(table)
}

