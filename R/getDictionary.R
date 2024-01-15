#' @export
getDictionary <- function(connection) {
  ids <- getIds(connection)
  dictionary <- list()
  for (name in names(ids)) {
    nameWithoutId <- sub("_id$", "", name)
    idsForName <- ids[[name]]
    idsString <- paste(idsForName, collapse = ", ")
    query <- paste0("SELECT * FROM ", nameWithoutId, " WHERE ", name, " IN (", idsString, ")")
    result <- tryCatch(
      {
        df <- DBI::dbGetQuery(connection, query)
        df <- df[, !grepl("source", names(df))]
        df
      },
      error = function(error) NULL
    )
    if (!is.null(result)) {
      dictionary[[nameWithoutId]] <- result
    }
  }
  return(dictionary)
}
