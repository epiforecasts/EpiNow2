% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dist_spec.R
\name{discretise}
\alias{discretise}
\alias{discretize}
\title{Discretise a <dist_spec>}
\usage{
discretise(x, silent = TRUE)

discretize(x, silent = TRUE)
}
\arguments{
\item{x}{A \verb{<dist_spec>}}

\item{silent}{Logical; if \code{TRUE} then any distribution that can't be
discretised will be returned as is. If \code{FALSE} then an error will be
thrown.}
}
\value{
A \verb{<dist_spec>} where all distributions with constant parameters are
nonparametric.
}
\description{
Discretise a <dist_spec>
}
\details{
By default it will discretise all the distributions it can discretise
(i.e. those with finite support and constant parameters).
}
\examples{
# A fixed gamma distribution with mean 5 and sd 1.
dist1 <- Gamma(mean = 5, sd = 1, max = 20)

# An uncertain lognormal distribution with mean 3 and sd 2
dist2 <- LogNormal(mean = Normal(3, 0.5), sd = Normal(2, 0.5), max = 20)

# The maxf the sum of two distributions
discretise(dist1 + dist2)
}
