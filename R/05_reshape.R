#' Reshape Table to Wide Format
#'
#' This function reshapes a given table to a wide format, which facilitates merging with the rest of the tables in the
#' database using the specified 'merge column'.
#'
#' It standardizes the names in the concept ID column since these names will become column names in the reshaped table and
#' must be compatible with the data frame column naming system.
#'
#' It also sequences repeated elements of the same type for the same person in the columns to separate these instances
#' for the reshaping process. This is necessary to handle repeated concept names and longitudinal data.
#'
#' The reshaping process uses the merge column as the identifier variable (idvar), which is typically "person_id" unless
#' another connecting element to the rest of the database is specified. The time variable (timevar) is the concept ID column,
#' characterizing the types of elements for each row in the table. This creates a merge column x concept ID column relationship.
#'
#' @param table A data frame representing the table to be reshaped.
#' @param conceptIdColumn A string specifying the name of the concept ID column, which characterizes the types of elements.
#' @param mergeColumn A string specifying the name of the merge column, typically "person_id", used as the identifier variable.
#' @param sequenceLongitudinal A logical flag indicating whether to sequence longitudinal data, defaults to FALSE.
#' 
#' @return A data frame in wide format with standardized and sequenced column names based on the concept ID column.
#'
reshapeTable <- function(table, conceptIdColumn, mergeColumn, sequenceLongitudinal = FALSE) {
  # Checks if the merge column is present in the table
  if (!mergeColumn %in% names(table)) {
    stop(paste0("The column '", mergeColumn, "' is not present in the table."))
  }

  # Sequences repeated data in the concept ID colum in case there is longitudinal data
  if(sequenceLongitudinal) {
    table <- sequenceColumn(table, conceptIdColumn, mergeColumn)
  }

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


#' Rearrange Column Name Tokens
#'
#' This function rearranges the tokens in the column names of a reshaped table. In the reshaped table,
#' column names consist of tokens separated by ".", where the first token refers to the original table column
#' and the second token refers to the related concept type. This function modifies the column names so that
#' the concept type appears first, followed by the original table column name, improving the readability
#' of the resulting columns.
#'
#' @param table A data frame with column names to be rearranged.
#'
#' @return A data frame with rearranged column names.
#'
rearrangeColumnNames <- function(table) {
  # Splits the column names into tokens
  tokens <- strsplit(names(table), "\\.")

  # Moves every first token to the end for each column name
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
