% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{make_X_trt_design}
\alias{make_X_trt_design}
\title{Make treatment design matrix}
\usage{
make_X_trt_design(dat, ctr = contr.equalprior)
}
\arguments{
\item{dat}{A dataset with factor variables `randA` and `randC`}

\item{ctr}{Contrast, by default equal marginals}
}
\value{
Design matrix
}
\description{
Setup the design matrix for interventions using contrast `ctr`.
}
