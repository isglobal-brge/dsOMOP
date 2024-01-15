#' @title Retrieve a dictionary of database tables
#' @description This function retrieves a dictionary of ID values based on the IDs identified from the database connection. It iterates over each ID, constructs a SQL query to fetch the corresponding table, and adds it to the dictionary. If a table cannot be fetched, it is ignored.
#' @param connection A DBI connection object to the database.
#' @return A list where each element is a data frame representing a table from the database. The names of the list elements are the names of the tables without the "_id" suffix.
getDictionary <- function(connection) {
  # Get the IDs from the database connection
  ids <- getIds(connection)
  dictionary <- list()
  for (name in names(ids)) {
    # Remove the "_id" suffix from the name
    nameWithoutId <- sub("_id$", "", name)
    # Get the IDs for the current name
    idsForName <- ids[[name]]
    # Convert the IDs to a string
    idsString <- paste(idsForName, collapse = ", ")
    # Construct a SQL query to fetch the table
    query <- paste0("SELECT * FROM ", nameWithoutId, " WHERE ", name, " IN (", idsString, ")")
    # Try to fetch the table
    result <- tryCatch(
      {
        # Execute the SQL query
        df <- DBI::dbGetQuery(connection, query)
        # Remove columns with "source" in their names
        df <- df[, !grepl("source", names(df))]
        df
      },
      # If an error occurs, return NULL
      error = function(error) NULL
    )
    # If the table was fetched successfully, add it to the dictionary
    if (!is.null(result)) {
      dictionary[[nameWithoutId]] <- result
    }
  }
  return(dictionary)
}
