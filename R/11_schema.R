#' Register a new schema query for a specific DBMS
#'
#' @param dbms The name of the DBMS (e.g., "postgresql", "mysql").
#' @param query The schema query to be registered.
#' @param override A logical value indicating whether to override an existing query for the DBMS.
#'
#' @export
registerSchemaQuery <- function(dbms, query, override = TRUE) {
  if (!is.character(dbms) || !is.character(query)) {
    stop("Both dbms and query must be character strings.")
  }
  if (!override && !is.null(private$.schemaQueries[[dbms]])) {
    stop(paste("A query for", dbms, "already exists. Use override = TRUE to replace it."))
  }
  private$.schemaQueries[[dbms]] <- query
}


#' Register multiple schema queries
#'
#' @param queries A named list of schema queries where names are DBMS names and values are queries.
#' @param override A logical value indicating whether to override existing queries for the DBMS.
#'
#' @export
registerSchemaQueries <- function(queries, override = TRUE) {
  if (!is.list(queries) || !all(sapply(queries, is.character))) {
    stop("Queries must be a named list of character strings.")
  }
  for (dbms in names(queries)) {
    registerSchemaQuery(dbms, queries[[dbms]], override)
  }
}


#' Get registered schema queries
#'
#' @return A named list of registered schema queries.
#'
#' @export
getSchemaQueries <- function() {
  return(private$.schemaQueries)
}


#' Get the schema query for a specific DBMS
#'
#' @param dbms The name of the DBMS (e.g., "postgresql", "mysql").
#' @return The schema query corresponding to the specified DBMS.
#' @export
getSchemaQuery <- function(dbms) {
  if (!is.character(dbms)) {
    stop("DBMS must be a character string.")
  }
  query <- private$.schemaQueries[[dbms]]
  if (is.null(query)) {
    stop(paste("No schema query found for DBMS:", dbms))
  }
  return(query)
}


#' Fill Schema Query
#'
#' @param schema The name of the schema to be used in the query.
#' @param query The schema query template containing the placeholder {schema}.
#' @return The schema query with the {schema} placeholder replaced by the actual schema name.
#' @export
fillSchemaQuery <- function(schema, query) {
  if (!is.character(schema) || !is.character(query)) {
    stop("Both schema and query must be character strings.")
  }
  filledQuery <- gsub("\\{schema\\}", schema, query)
  return(filledQuery)
}



# Initialize the private schemaQueries variable
private <- new.env()
private$.schemaQueries <- list()

# Register the schema queries for the supported DBMS
registerSchemaQueries(
  queries = list(
    "postgresql" = "SET search_path TO {schema}",
    "mysql" = "USE {schema}",
    "mariadb" = "USE {schema}"
  ),
  override = TRUE
)
