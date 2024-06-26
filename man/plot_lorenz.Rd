% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_lorenz.R
\name{plot_lorenz}
\alias{plot_lorenz}
\title{Plot Gini index evolution on microsimulated series}
\usage{
plot_lorenz(
  simulations,
  year_var = "annee",
  year = 2020,
  wealth_var = "wealth",
  labor_income_var = "revenu",
  total_income_var = "Y",
  export_plot = FALSE,
  export_path = getwd(),
  export_filename = "05_lorenz",
  langage = c("French", "English"),
  scale_viridis = FALSE,
  negative_values = c("truncate", "keep", "remove"),
  ...
)
}
\arguments{
\item{simulations}{Longitudinal household level dataframe
storing wealth simulations}

\item{year_var}{Year variable}

\item{year}{Year to keep. Only valid for \code{plot_rK_income_1y}}

\item{export_plot}{Logical value indicating whether we want
to export the plot (\code{TRUE}, default) or not}

\item{export_path}{Path to export data}

\item{export_filename}{Filename if data are exported. PDF extension
will be added if missing}

\item{langage}{Should we label axes in English or French ?}

\item{scale_viridis}{Logical value. Should we use viridis (\code{TRUE})
or not not (\code{FALSE})}

\item{...}{Additional arguments to customize \code{ggplot2} call}

\item{vars}{Variables that should be represented}

\item{labels}{Variable labels for legend}
}
\value{
A ggplot object
}
\description{
Determine evolution of Gini index
}
