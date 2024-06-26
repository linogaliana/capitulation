% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example_life_cycle.R
\name{example_life_cycle}
\alias{example_life_cycle}
\title{Simple example to illustrate life cycle
 dynamic}
\usage{
example_life_cycle(
  r = 0.02,
  gamma = 0.5,
  beta = 1,
  K0 = c(NULL, 0),
  K2009 = 100,
  time_0 = c("graduation", "birth"),
  income_seq = NULL,
  seed = NULL,
  return_data = FALSE
)
}
\arguments{
\item{r}{Exogeneous interest rate}

\item{gamma}{Risk aversion coefficient}

\item{beta}{Utility discount factor}

\item{K0}{Initial capital that should be used. If set to
\code{NULL}, deduced recursively given \code{K2009} value}

\item{K2009}{Wealth measured in survey data for this
example individual}
}
\value{
\code{ggplot} object
}
\description{
Simple example to illustrate life cycle
 dynamic
}
\examples{
\dontrun{example_life_cycle(K0=0)}
}
