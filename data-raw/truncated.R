library(EpiNow2)

# Expose Stan functions to use discretised_pmf directly
expose_stan_fns(
  "pmfs.stan",
  target_dir = system.file("stan", "functions", package = "EpiNow2")
)

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

  # Use Stan discretised_pmf directly (dist=0 for lognormal)
  # nolint start: object_usage_linter
  pmf <- discretised_pmf(c(meanlog, sdlog), max_d + 1L, 0L)
  # nolint end
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
