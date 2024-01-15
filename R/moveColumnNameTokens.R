#' @export
moveColumnNameTokens <- function(table) {
  tokens <- strsplit(names(table), "\\.")
  movedTokens <- lapply(tokens, function(token) {
    if (length(token) > 1) {
      token <- c(token[-1], token[1])
    }
    return(token)
  })
  columnNames <- sapply(movedTokens, paste, collapse = ".")
  names(table) <- columnNames
  return(table)
}
