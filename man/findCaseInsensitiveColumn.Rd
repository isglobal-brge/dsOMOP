% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_columns.R
\name{findCaseInsensitiveColumn}
\alias{findCaseInsensitiveColumn}
\title{Find Column Names with Case-Insensitive Matching}
\usage{
findCaseInsensitiveColumn(columnNames, target)
}
\arguments{
\item{columnNames}{Character vector of available column names to search through}

\item{target}{Character string of the target column name to find}
}
\value{
Character string of the matched column name or NULL if no match found
}
\description{
Performs robust column name matching with support for both exact and case-insensitive
matches. Prioritizes exact matches before falling back to case-insensitive comparison.
}
\details{
Case-Insensitive Column Name Matching

The matching process follows this hierarchy:
\enumerate{
\item Attempts exact string matching first
\item If no exact match is found, performs case-insensitive matching
\item Returns NULL if no match is found at either level
}

This approach is particularly useful for:
\itemize{
\item Supporting flexible column name references
\item Maintaining backward compatibility
\item Handling inconsistent column name casing
}
}
\examples{
\dontrun{
cols <- c("PersonId", "BirthDate", "Gender")

# Exact match
findCaseInsensitiveColumn(cols, "PersonId")  # Returns "PersonId"

# Case-insensitive match
findCaseInsensitiveColumn(cols, "personid")  # Returns "PersonId"

# No match
findCaseInsensitiveColumn(cols, "Age")       # Returns NULL
}

}
