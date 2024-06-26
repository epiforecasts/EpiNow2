% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regional_epinow.R
\name{process_region}
\alias{process_region}
\title{Process regional estimate}
\usage{
process_region(
  out,
  target_region,
  timing,
  return_output = TRUE,
  return_timing = TRUE,
  complete_logger = "EpiNow2.epinow"
)
}
\arguments{
\item{out}{List of output returned by \code{\link[=epinow]{epinow()}}}

\item{target_region}{Character string indicating the region being evaluated}

\item{timing}{Output from \code{\link[=Sys.time]{Sys.time()}}}

\item{return_output}{Logical, defaults to FALSE. Should output be returned,
this automatically updates to TRUE if no directory for saving is specified.}

\item{return_timing}{Logical, should runtime be returned}

\item{complete_logger}{Character string indicating the logger to output
the completion of estimation to.}
}
\value{
A list of processed output
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#maturing}{\figure{lifecycle-maturing.svg}{options: alt='[Maturing]'}}}{\strong{[Maturing]}}
Internal function that removes output that is not required, and returns
logging information.
}
\seealso{
\code{\link[=regional_epinow]{regional_epinow()}}
}
\keyword{internal}
