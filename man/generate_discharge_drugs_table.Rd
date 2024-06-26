% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_functions.R
\name{generate_discharge_drugs_table}
\alias{generate_discharge_drugs_table}
\title{Drugs used during hospital stay table}
\usage{
generate_discharge_drugs_table(dat, format = "html")
}
\arguments{
\item{dat}{The dataset}

\item{closed}{If TRUE, generate overall table only, if FALSE one table per domain}
}
\value{
A tibble giving the summary
}
\description{
Generates summary table of drugs used during hospital stay,
as recorded on discharge, by domain or overall.
}
