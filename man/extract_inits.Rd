% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{extract_inits}
\alias{extract_inits}
\title{Generate initial conditions from a Stan fit}
\usage{
extract_inits(fit, current_inits, exclude_list = NULL, samples = 50)
}
\arguments{
\item{fit}{A \verb{<stanfit>} object.}

\item{current_inits}{A function that returns a list of initial conditions
(such as \code{\link[=create_initial_conditions]{create_initial_conditions()}}). Only used in \code{exclude_list} is
specified.}

\item{exclude_list}{A character vector of parameters to not initialise from
the fit object, defaulting to \code{NULL}.}

\item{samples}{Numeric, defaults to 50. Number of posterior samples.}
}
\value{
A function that when called returns a set of initial conditions as a
named list.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
Extracts posterior samples to use to initialise a full model fit. This may
be useful for certain data sets where the sampler gets stuck or cannot
easily be initialised. In \code{\link[=estimate_infections]{estimate_infections()}}, \code{\link[=epinow]{epinow()}} and
\code{\link[=regional_epinow]{regional_epinow()}} this option can be engaged by setting
\verb{stan_opts(init_fit = <stanfit>)}.

This implementation is based on the approach taken in
\href{https://github.com/ImperialCollegeLondon/epidemia/}{epidemia} authored by
James Scott.
}
