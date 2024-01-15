#' @title Move column name tokens
#' @description This function rearranges the tokens in the column names of a given table.
#' The tokens are assumed to be separated by periods. If a column name contains more than one token,
#' the function moves the first token to the end. The rearranged tokens are then joined back together
#' with periods to form the new column names.
#' @param table A data frame whose column names are to be rearranged.
#' @return A data frame with rearranged column names.
moveColumnNameTokens <- function(table) {
  # Split the column names into tokens
  tokens <- strsplit(names(table), "\\.")
  # Rearrange the tokens for each column name
  movedTokens <- lapply(tokens, function(token) {
    if (length(token) > 1) {
      token <- c(token[-1], token[1])
    }
    return(token)
  })

  # Join the rearranged tokens back together to form the new column names
  columnNames <- sapply(movedTokens, paste, collapse = ".")

  # Assign the new column names to the table
  names(table) <- columnNames

  return(table)
}
