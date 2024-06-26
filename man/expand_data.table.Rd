% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expand_DT.R
\name{expand_data.table}
\alias{expand_data.table}
\title{Expand data.table to prepare unnesting}
\usage{
expand_data.table(DT, listCol = "x", idCol = "Id")
}
\arguments{
\item{DT}{\link[data.table]{data.table} object}

\item{listCol}{Nested column}

\item{idCol}{Id column that should be exploded}
}
\value{
\link[data.table]{data.table} object with
 just one column exploded
}
\description{
Expand data.table to prepare unnesting
}
