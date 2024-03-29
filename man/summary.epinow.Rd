% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise.R
\name{summary.epinow}
\alias{summary.epinow}
\alias{summary}
\title{Summary output from epinow}
\usage{
\method{summary}{epinow}(
  object,
  output = c("estimates", "forecast", "estimated_reported_cases"),
  date = NULL,
  params = NULL,
  ...
)
}
\arguments{
\item{object}{A list of output as produced by "epinow".}

\item{output}{A character string of output to summarise. Defaults to
"estimates" but also supports "forecast", and "estimated_reported_cases".}

\item{date}{A date in the form "yyyy-mm-dd" to inspect estimates for.}

\item{params}{A character vector of parameters to filter for.}

\item{...}{Pass additional summary arguments to lower level methods}
}
\value{
Returns a \verb{<data.frame>} of summary output
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
\code{summary} method for class "epinow".
}
\seealso{
summary.estimate_infections epinow
}
