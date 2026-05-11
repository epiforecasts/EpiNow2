library(EpiNow2)

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

  # Only lognormal truncation is supported
  if (!identical(dist$distribution, "lognormal")) {
    cli::cli_abort(
      "apply_truncation currently supports lognormal truncation only."
    )
  }

  # Use fixed parameters if provided, otherwise use prior means
  if (is.null(meanlog)) {
    meanlog <- get_parameters(get_parameters(dist)$meanlog)$mean
  }
  if (is.null(sdlog)) {
    sdlog <- get_parameters(get_parameters(dist)$sdlog)$mean
  }

  # Use primarycensored to compute the PMF, consistent with the
  # Stan model's discretised_pmf (which uses primarycensored_sone_pmf_vectorized
  # with pwindow=1, uniform primary, D=max_d+1).
  pmf <- primarycensored::dprimarycensored(
    0:max_d, pdist = plnorm,
    pwindow = 1, swindow = 1, D = max_d + 1,
    meanlog = meanlog, sdlog = sdlog
  )
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
