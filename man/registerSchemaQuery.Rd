% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/11_schema.R
\name{registerSchemaQuery}
\alias{registerSchemaQuery}
\title{Register a new schema query for a specific DBMS}
\usage{
registerSchemaQuery(dbms, query, override = TRUE)
}
\arguments{
\item{dbms}{The name of the DBMS (e.g., "postgresql", "mysql").}

\item{query}{The schema query to be registered.}

\item{override}{A logical value indicating whether to override an existing query for the DBMS.}
}
\description{
Register a new schema query for a specific DBMS
}
