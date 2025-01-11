#' Register a new schema query for a specific DBMS
#'
#' This function allows registering a SQL query that will be used to set/change the schema
#' for a specific database management system (DBMS). The query should contain a {schema}
#' placeholder that will be replaced with the actual schema name when executed.
#'
#' @param dbms The name of the DBMS (e.g., "postgresql", "mysql", "mariadb").
#' @param query The schema query to be registered. Should contain {schema} placeholder.
#' @param override A logical value indicating whether to override an existing query for the DBMS.
#'                Default is TRUE.
#'
#' @examples
#' # Register a schema query for PostgreSQL
#' registerSchemaQuery("postgresql", "SET search_path TO {schema}")
#'
#' # Register without overriding existing query
#' \dontrun{
#' registerSchemaQuery("mysql", "USE {schema}", override = FALSE)
#' }
#'
#' @export
registerSchemaQuery <- function(dbms, query, override = TRUE) {
  # Input validation
  if (!is.character(dbms) || !is.character(query)) {
    stop("Both dbms and query must be character strings.")
  }
  
  # Check if query exists and override is FALSE
  if (!override && !is.null(private$.schemaQueries[[dbms]])) {
    stop(paste("A query for", dbms, "already exists. Use override = TRUE to replace it."))
  }
  
  # Store the query in the private environment
  private$.schemaQueries[[dbms]] <- query
}

#' Register multiple schema queries at once
#'
#' This function allows bulk registration of schema queries for multiple DBMS systems.
#' It provides a convenient way to register multiple queries in one call.
#'
#' @param queries A named list of schema queries where names are DBMS names and values are queries.
#'                Each query should contain a {schema} placeholder.
#' @param override A logical value indicating whether to override existing queries for the DBMS.
#'                Default is TRUE.
#'
#' @examples
#' # Register queries for multiple DBMS systems
#' queries <- list(
#'   postgresql = "SET search_path TO {schema}",
#'   mysql = "USE {schema}",
#'   mariadb = "USE {schema}"
#' )
#' registerSchemaQueries(queries)
#'
#' @export
registerSchemaQueries <- function(queries, override = TRUE) {
  # Validate input format
  if (!is.list(queries) || !all(sapply(queries, is.character))) {
    stop("Queries must be a named list of character strings.")
  }
  
  # Register each query individually
  for (dbms in names(queries)) {
    registerSchemaQuery(dbms, queries[[dbms]], override)
  }
}

#' Get all registered schema queries
#'
#' Retrieves all currently registered schema queries across all supported DBMS systems.
#'
#' @return A named list of registered schema queries, where names are DBMS identifiers
#'         and values are the corresponding schema queries.
#'
#' @examples
#' # Get all registered queries
#' queries <- getSchemaQueries()
#' print(queries)
#'
#' @export
getSchemaQueries <- function() {
  return(private$.schemaQueries)
}

#' Get the schema query for a specific DBMS
#'
#' Retrieves the registered schema query for a particular database management system.
#'
#' @param dbms The name of the DBMS (e.g., "postgresql", "mysql", "mariadb").
#' @return The schema query string corresponding to the specified DBMS.
#'
#' @examples
#' # Get PostgreSQL schema query
#' query <- getSchemaQuery("postgresql")
#' print(query)  # Should print: SET search_path TO {schema}
#'
#' @export
getSchemaQuery <- function(dbms) {
  # Input validation
  if (!is.character(dbms)) {
    stop("DBMS must be a character string.")
  }
  
  # Retrieve and validate query existence
  query <- private$.schemaQueries[[dbms]]
  if (is.null(query)) {
    stop(paste("No schema query found for DBMS:", dbms))
  }
  
  return(query)
}

#' Fill Schema Query with actual schema name
#'
#' Replaces the {schema} placeholder in a schema query template with the actual schema name.
#' This function is used internally to prepare the final query for execution.
#'
#' @param schema The name of the schema to be used in the query.
#' @param query The schema query template containing the placeholder {schema}.
#' @return The schema query with the {schema} placeholder replaced by the actual schema name.
#'
#' @examples
#' # Fill a PostgreSQL schema query
#' template <- "SET search_path TO {schema}"
#' filled <- fillSchemaQuery("public", template)
#' print(filled)  # Prints: "SET search_path TO public"
#'
#' @export
fillSchemaQuery <- function(schema, query) {
  # Input validation
  if (!is.character(schema) || !is.character(query)) {
    stop("Both schema and query must be character strings.")
  }
  
  # Replace placeholder with actual schema name
  filledQuery <- gsub("\\{schema\\}", schema, query)
  return(filledQuery)
}

#' Register a new schema retrieval query for a specific DBMS
#'
#' This function registers a SQL query that will be used to retrieve the current schema
#' for a specific database management system (DBMS).
#'
#' @param dbms The name of the DBMS (e.g., "postgresql", "mysql", "mariadb").
#' @param query The schema retrieval query to be registered.
#' @param override A logical value indicating whether to override an existing query for the DBMS.
#'                Default is TRUE.
#'
#' @examples
#' # Register a schema retrieval query for PostgreSQL
#' registerSchemaRetrievalQuery("postgresql", "SHOW search_path")
#'
#' # Register without overriding
#' \dontrun{
#' registerSchemaRetrievalQuery("mysql", "SELECT DATABASE()", override = FALSE)
#' }
#'
#' @export
registerSchemaRetrievalQuery <- function(dbms, query, override = TRUE) {
  # Input validation
  if (!is.character(dbms) || !is.character(query)) {
    stop("Both dbms and query must be character strings.")
  }
  
  # Check for existing query when override is FALSE
  if (!override && !is.null(private$.schemaRetrievalQueries[[dbms]])) {
    stop(paste("A retrieval query for", dbms, "already exists. Use override = TRUE to replace it."))
  }
  
  # Store the query
  private$.schemaRetrievalQueries[[dbms]] <- query
}

#' Register multiple schema retrieval queries at once
#'
#' This function allows bulk registration of schema retrieval queries for multiple DBMS systems.
#'
#' @param queries A named list of schema retrieval queries where names are DBMS names and values are queries.
#' @param override A logical value indicating whether to override existing queries for the DBMS.
#'                Default is TRUE.
#'
#' @examples
#' # Register retrieval queries for multiple DBMS systems
#' queries <- list(
#'   postgresql = "SHOW search_path",
#'   mysql = "SELECT DATABASE()",
#'   mariadb = "SELECT DATABASE()"
#' )
#' registerSchemaRetrievalQueries(queries)
#'
#' @export
registerSchemaRetrievalQueries <- function(queries, override = TRUE) {
  # Validate input format
  if (!is.list(queries) || !all(sapply(queries, is.character))) {
    stop("Queries must be a named list of character strings.")
  }
  
  # Register each query individually
  for (dbms in names(queries)) {
    registerSchemaRetrievalQuery(dbms, queries[[dbms]], override)
  }
}

#' Get all registered schema retrieval queries
#'
#' Retrieves all currently registered schema retrieval queries across all supported DBMS systems.
#'
#' @return A named list of registered schema retrieval queries, where names are DBMS identifiers
#'         and values are the corresponding retrieval queries.
#'
#' @examples
#' # Get all registered retrieval queries
#' queries <- getSchemaRetrievalQueries()
#' print(queries)
#'
#' @export
getSchemaRetrievalQueries <- function() {
  return(private$.schemaRetrievalQueries)
}

#' Get the schema retrieval query for a specific DBMS
#'
#' Retrieves the registered schema retrieval query for a particular database management system.
#'
#' @param dbms The name of the DBMS (e.g., "postgresql", "mysql", "mariadb").
#' @return The schema retrieval query string corresponding to the specified DBMS.
#'
#' @examples
#' # Get PostgreSQL schema retrieval query
#' query <- getSchemaRetrievalQuery("postgresql")
#' print(query)  # Should print "SHOW search_path"
#'
#' @export
getSchemaRetrievalQuery <- function(dbms) {
  # Input validation
  if (!is.character(dbms)) {
    stop("DBMS must be a character string.")
  }
  
  # Retrieve and validate query existence
  query <- private$.schemaRetrievalQueries[[dbms]]
  if (is.null(query)) {
    stop(paste("No schema retrieval query found for DBMS:", dbms))
  }
  
  return(query)
}

# Initialize private environment for storing queries
private <- new.env()
private$.schemaQueries <- list()           # Store schema setting queries
private$.schemaRetrievalQueries <- list()  # Store schema retrieval queries

# Register default schema queries for supported DBMS systems
registerSchemaQueries(
  queries = list(
    "postgresql" = "SET search_path TO {schema}",
    "mysql" = "USE {schema}",
    "mariadb" = "USE {schema}"
  ),
  override = TRUE
)

# Register default schema retrieval queries for supported DBMS systems
registerSchemaRetrievalQueries(
  queries = list(
    "postgresql" = "SHOW search_path",
    "mysql" = "SELECT DATABASE()",
    "mariadb" = "SELECT DATABASE()"
  ),
  override = TRUE
)
