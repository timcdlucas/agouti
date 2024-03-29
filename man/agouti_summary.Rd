% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{agouti_summary}
\alias{agouti_summary}
\title{Summarise a aggregated output dataset.}
\usage{
agouti_summary(x, ID = ID, high_res = NA, na.rm = FALSE)
}
\arguments{
\item{x}{A data frame suitable for use as aggregate output data.}

\item{ID}{The column to use as the group ID column. Unquoted value.}

\item{high_res}{A string or vector of strings specifying a column or columns of high resolution variables to summarise.
default is NULL in which case the function will summarise all variables with more unique values than ID}

\item{na.rm}{default is false, if na.rm is not equal to false, NA's will be dropped}
}
\description{
Summarise a aggregated output dataset.
}
\examples{
data(madagascar_malaria)
agouti_summary(madagascar_malaria)
agouti_summary(madagascar_malaria, high_res=c("pop","EVI"))
}
