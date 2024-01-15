#' @export
sortTableChronologically <- function(table, timeColumn) {
  table <- table[order(table[[timeColumn]]), ]
  return(table)
}
