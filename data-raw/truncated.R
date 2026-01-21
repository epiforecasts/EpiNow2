library(EpiNow2)

#' Discretised lognormal PMF using CDF-based discretisation
#'
#' Computes P(i-1 < X <= i) for i = 1, 2, ..., max_d+1
#' This matches the discretisation used in R/utilities.R
#'
#' @param meanlog Mean on log scale
#' @param sdlog SD on log scale
#' @param max_d Maximum delay
#' @return Normalised PMF vector
#' @keywords internal
discretised_lognormal_pmf <- function(meanlog, sdlog, max_d) {
  pmf <- plnorm(1:(max_d + 1), meanlog, sdlog) -
    plnorm(0:max_d, meanlog, sdlog)
  pmf / plnorm(max_d + 1, meanlog, sdlog)
}

#' Apply truncation to a data set
#'
#' @param index Index from which to truncate
#' @param data Data set
#' @param dist Truncation distribution
#' @param meanlog Fixed meanlog parameter (uses prior mean if NULL)
#' @param sdlog Fixed sdlog parameter (uses prior mean if NULL)
#'
#' @return A truncated data set
#' @keywords internal
apply_truncation <- function(index, data, dist, meanlog = NULL, sdlog = NULL) {
  max_d <- max(dist)

  # Use fixed parameters if provided, otherwise use prior means
  if (is.null(meanlog)) {
    meanlog <- get_parameters(get_parameters(dist)$meanlog)$mean
  }
  if (is.null(sdlog)) {
    sdlog <- get_parameters(get_parameters(dist)$sdlog)$mean
  }

  # Use CDF-based discretisation (matches R/utilities.R discretised_lognormal_pmf)
  pmf <- discretised_lognormal_pmf(meanlog, sdlog, max_d)
  cmf <- cumsum(pmf)
  cmf <- rev(cmf)[-1]

  trunc_data <- data.table::copy(data)[1:(.N - index)]
  trunc_data[
    (.N - length(cmf) + 1):.N, confirm := as.integer(confirm * cmf)
  ]
  trunc_data
}

# get example case counts
reported_cases <- example_confirmed[1:60]

# define example truncation distribution (note not integer adjusted)
trunc <- LogNormal(
  meanlog = Normal(0.9, 0.1),
  sdlog = Normal(0.6, 0.1),
  max = 10
)

# Use the make_truncated_data() function to generate example data for
# an example using estimate_truncation()
example_truncated <- purrr::map(
  seq(20, 0, -5),
  apply_truncation,
  data = reported_cases,
  dist = trunc
)

usethis::use_data(
  example_truncated,
  overwrite = TRUE,
  compress = "xz"
)
