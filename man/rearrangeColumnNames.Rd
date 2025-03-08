% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/05_reshape.R
\name{rearrangeColumnNames}
\alias{rearrangeColumnNames}
\title{Column Name Standardization for Reshaped Tables}
\usage{
rearrangeColumnNames(table)
}
\arguments{
\item{table}{A data frame containing reshaped OMOP CDM data with composite column names}
}
\value{
A data frame with standardized column names following the concept-first convention
}
\description{
Standardizes and rearranges column names in reshaped OMOP CDM tables to improve readability
and maintain consistent naming conventions. Handles the reorganization of composite column
names created during the reshaping process.
}
\details{
Rearrange Column Names for Reshaped OMOP CDM Tables

The function performs the following operations:
\enumerate{
\item Splits column names into constituent tokens
\item Reorders tokens to prioritize concept information
\item Reconstructs column names with standardized structure
}

Column name transformation process:
\itemize{
\item Original format: "measurement.concept"
\item Transformed format: "concept.measurement"
}

This standardization is important for:
\itemize{
\item Maintaining consistent naming conventions
\item Improving column name readability
\item Facilitating data analysis and filtering
\item Supporting automated processing
}
}
\examples{
\dontrun{
# Example with reshaped data
df <- data.frame(
  person_id = 1:3,
  value.diabetes = c(1, 0, 1),
  date.diabetes = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
)

# Standardize column names
df_standard <- rearrangeColumnNames(df)
# Results in: person_id, diabetes.value, diabetes.date
}

}
