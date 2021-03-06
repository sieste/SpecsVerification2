% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EnsQs.R
\name{EnsQs}
\alias{EnsQs}
\title{Calculate the ensemble-adjusted Quadratic Score (QS) for categorical forecasts}
\usage{
EnsQs(ens, obs, R.new = NA)
}
\arguments{
\item{ens}{a N*R matrix of integers, representing N time instances of categorical ensemble forecasts; ens[t,r] indicates the category index that the r-th ensemble member forecasts at time t}

\item{obs}{a vector of length N, obs[t] is the category that occurred at time t}

\item{R.new}{ensemble size for which the scores should be adjusted}
}
\value{
numeric vector of length N with the ensemble-adjusted quadratic score values
}
\description{
Calculate the ensemble-adjusted Quadratic Score (QS) for categorical forecasts
}
\details{
It is assumed that the smallest class index is 1, and the largest class index is calculated by max(c(ens,obs))
}
\examples{
data(eurotempforecast)
EnsQs(ens.cat, obs.cat, R.new=Inf)
}
\references{
n/a
}
\seealso{
EnsBrier, EnsRps, EnsCrps, ScoreDiff, SkillScore
}

