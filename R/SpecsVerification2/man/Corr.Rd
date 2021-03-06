% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Corr.R
\name{Corr}
\alias{Corr}
\title{Calculate correlation between forecasts and observations, and assess uncertainty}
\usage{
Corr(fcst, obs, N.eff = NA, conf.level = 0.95, handle.na = "na.fail")
}
\arguments{
\item{fcst}{vector of forecasts}

\item{obs}{vector of observations}

\item{N.eff}{user-defined effective sample size to be used in hypothesis test and for confidence bounds; if NA, the length of `obs` is used after removing missing values; default: NA}

\item{conf.level}{confidence level used the confidence interval; default = 0.95}

\item{handle.na}{how should missing values in forecasts and observations be handled; possible values are 'na.fail' and 'use.pairwise.complete'; default: 'na.fail'}
}
\value{
vector with correlation, one-sided p-value, and central confidence interval at the user-defined confidence level
}
\description{
Calculate correlation between forecasts and observations, and assess uncertainty
}
\examples{
data(eurotempforecast)
Corr(rowMeans(ens), obs)
}
\references{
Von Storch, Zwiers (2001): Statistical analysis in climate research. Cambridge University Press.
}
\seealso{
CorrDiff
}

