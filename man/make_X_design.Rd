% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{make_X_design}
\alias{make_X_design}
\title{Make design matrix}
\usage{
make_X_design(
  dat,
  vars = NULL,
  ctr = contr.equalprior,
  includeA = TRUE,
  includeC = TRUE,
  intercept = TRUE
)
}
\arguments{
\item{dat}{A dataset}

\item{vars}{String vector of other columns to include}

\item{includeA}{Include domain A design}

\item{includeC}{Include domain C design}

\item{intercept}{Include an intercept term}
}
\value{
A design matrix
}
\description{
Setup the design matrix for treatments and other covariates as required
}
