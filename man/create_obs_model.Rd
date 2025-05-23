% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create.R
\name{create_obs_model}
\alias{create_obs_model}
\title{Create Observation Model Settings}
\usage{
create_obs_model(obs = obs_opts(), dates)
}
\arguments{
\item{obs}{A list of options as generated by \code{\link[=obs_opts]{obs_opts()}} defining the
observation model. Defaults to \code{\link[=obs_opts]{obs_opts()}}.}

\item{dates}{A vector of dates used to calculate the day of the week.}
}
\value{
A list of settings ready to be passed to stan defining
the Observation Model
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
Takes the output of \code{\link[=obs_opts]{obs_opts()}} and converts it into a list understood
by stan.
}
\examples{
\dontrun{
dates <- seq(as.Date("2020-03-15"), by = "days", length.out = 15)
# default observation model data
create_obs_model(dates = dates)

# Poisson observation model
create_obs_model(obs_opts(family = "poisson"), dates = dates)

# Applying a observation scaling to the data
create_obs_model(
  obs_opts(scale = Normal(mean = 0.4, sd = 0.01)),
  dates = dates
)

# Apply a custom week week length
create_obs_model(obs_opts(week_length = 3), dates = dates)
}
}
\seealso{
\code{\link[=obs_opts]{obs_opts()}}
}
\keyword{internal}
