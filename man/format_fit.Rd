% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_infections.R
\name{format_fit}
\alias{format_fit}
\title{Format Posterior Samples}
\usage{
format_fit(posterior_samples, horizon, shift, burn_in, start_date, CrIs)
}
\arguments{
\item{posterior_samples}{A list of posterior samples as returned by
\code{\link[=extract_parameter_samples]{extract_parameter_samples()}}.}

\item{horizon}{Numeric, forecast horizon.}

\item{shift}{Numeric, the shift to apply to estimates.}

\item{burn_in}{Deprecated; this functionality is no longer available.}

\item{start_date}{Deprecated; this functionality is no longer available.}

\item{CrIs}{Numeric vector of credible intervals to calculate.}
}
\value{
A list of samples and summarised posterior parameter estimates.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
Summaries posterior samples and adds additional custom variables.
}
\keyword{internal}
