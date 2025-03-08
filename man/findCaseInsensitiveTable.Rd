% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_table.R
\name{findCaseInsensitiveTable}
\alias{findCaseInsensitiveTable}
\title{Find Table Name with Case-Insensitive Matching}
\usage{
findCaseInsensitiveTable(tableNames, target)
}
\arguments{
\item{tableNames}{Character vector of available table names}

\item{target}{Character string of the target table name to find}
}
\value{
Character string of the matched table name or NULL if no match found
}
\description{
Performs case-insensitive matching of table names to support flexible table name references.
Prioritizes exact matches before falling back to case-insensitive matching.
}
\details{
Case-Insensitive Table Name Matching
}
\examples{
\dontrun{
tables <- c("Person", "Measurement", "Observation")

# Exact match
findCaseInsensitiveTable(tables, "Person")  # Returns "Person"

# Case-insensitive match
findCaseInsensitiveTable(tables, "person")  # Returns "Person"

# No match
findCaseInsensitiveTable(tables, "Drug")    # Returns NULL
}

}
