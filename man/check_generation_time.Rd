% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{check_generation_time}
\alias{check_generation_time}
\title{Validate probability distribution for using as generation time}
\usage{
check_generation_time(dist)
}
\arguments{
\item{dist}{A \code{dist_spec} object.`}
}
\value{
Called for its side effects.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
does all the checks in\code{check_stan_delay()} and additionally makes sure
that if \code{dist} is nonparametric,  its first element is zero.
}
\keyword{internal}
