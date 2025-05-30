% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{plot_estimates}
\alias{plot_estimates}
\title{Plot Estimates}
\usage{
plot_estimates(
  estimate,
  reported,
  ylab,
  hline,
  obs_as_col = TRUE,
  max_plot = 10,
  estimate_type = c("Estimate", "Estimate based on partial data", "Forecast")
)
}
\arguments{
\item{estimate}{A \verb{<data.table>} of estimates containing the following
variables: date, type (must contain "estimate", "estimate based on partial
data" and optionally "forecast").}

\item{reported}{A \verb{<data.table>} of reported cases with the following
variables: date, confirm.}

\item{ylab}{Character string. Title for the plot y
axis.}

\item{hline}{Numeric, if supplied gives the horizontal intercept for a
indicator line.}

\item{obs_as_col}{Logical, defaults to \code{TRUE}. Should observed data, if
supplied, be plotted using columns or as points (linked using a line).}

\item{max_plot}{Numeric, defaults to 10. A multiplicative upper bound on the\
number of cases shown on the plot. Based on the maximum number of reported
cases.}

\item{estimate_type}{Character vector indicating the type of data to plot.
Default to all types with supported options being: "Estimate", "Estimate
based on partial data", and "Forecast".}
}
\value{
A \code{ggplot2} object
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#questioning}{\figure{lifecycle-questioning.svg}{options: alt='[Questioning]'}}}{\strong{[Questioning]}}
Allows users to plot the output from \code{\link[=estimate_infections]{estimate_infections()}} easily.
In future releases it may be depreciated in favour of increasing the
functionality of the S3 plot methods.
}
\examples{
# get example model results
out <- readRDS(system.file(
  package = "EpiNow2", "extdata", "example_estimate_infections.rds"
))

# plot infections
plot_estimates(
  estimate = out$summarised[variable == "infections"],
  reported = out$observations,
  ylab = "Cases", max_plot = 2
) + ggplot2::facet_wrap(~type, scales = "free_y")

# plot reported cases estimated via Rt
plot_estimates(
  estimate = out$summarised[variable == "reported_cases"],
  reported = out$observations,
  ylab = "Cases"
)

# plot Rt estimates
plot_estimates(
  estimate = out$summarised[variable == "R"],
  ylab = "Effective Reproduction No.",
  hline = 1
)

#' # plot Rt estimates without forecasts
plot_estimates(
  estimate = out$summarised[variable == "R"],
  ylab = "Effective Reproduction No.",
  hline = 1, estimate_type = "Estimate"
)
}
