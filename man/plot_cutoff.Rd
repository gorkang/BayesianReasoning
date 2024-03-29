% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_cutoff.R
\name{plot_cutoff}
\alias{plot_cutoff}
\title{plot_cutoff
Create a cutoff plot, showing the healthy and sick distributions, and the 
consequences of different cutoff points}
\usage{
plot_cutoff(
  prevalence = 0.1,
  cutoff_point = 30,
  mean_sick = 35,
  mean_healthy = 20,
  sd_sick = 3,
  sd_healthy = 5,
  n_people = 1e+05,
  add_table = FALSE,
  output_filename = NULL
)
}
\arguments{
\item{prevalence}{prevalence of the disease}

\item{cutoff_point}{cutoff point to use}

\item{mean_sick}{mean for the sick people distribution}

\item{mean_healthy}{mean for the healthy people distribution}

\item{sd_sick}{sd for the sick people distribution}

\item{sd_healthy}{sd for the healthy people distribution}

\item{n_people}{number of people to use}

\item{add_table}{FALSE/TRUE: add gt table with Sensitivity, Specificity, etc.}

\item{output_filename}{NULL. If a filename, will save the plot}
}
\value{
A list with plots and table
}
\description{
plot_cutoff
Create a cutoff plot, showing the healthy and sick distributions, and the 
consequences of different cutoff points
}
\examples{
\dontrun{
plot_cutoff(prevalence = 0.2)
}
}
